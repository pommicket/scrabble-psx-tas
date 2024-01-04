use crate::{
	board::{Board, PlayedWord, Square},
	play::{parse_play_structure, Play, PlayStructure},
	tile::{Tile, TileBag},
	word_list::WordList,
};
use anyhow::Context;
use std::{
	fs::{self, File},
	io::{self, prelude::*},
	sync::Arc,
};

#[derive(Debug, Clone, Default)]
pub struct Rng {
	seed: u32,
	calls: u32,
}

fn next_rand(seed: u32) -> u32 {
	seed.wrapping_mul(0x41c64e6d).wrapping_add(12345)
}

impl Rng {
	pub fn skip(&mut self, count: u32) {
		for _ in 0..count {
			self.seed = next_rand(self.seed);
		}
		self.calls += count;
	}
	pub fn skip_to(&mut self, call: u32) {
		assert!(call >= self.calls);
		self.skip(call - self.calls);
	}
	#[allow(clippy::should_implement_trait)]
	pub fn next(&mut self) -> u32 {
		self.skip(1);
		(self.seed >> 16) & 0x7fff
	}
}

pub struct MovieWriter {
	out: Option<zip::ZipWriter<io::BufWriter<File>>>,
	log: Option<io::BufWriter<File>>,
	frames: u32,
	finished: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Button {
	U,
	D,
	L,
	R,
	T,
	X,
	Q,
	O,
}

impl Button {
	const fn index(self) -> u8 {
		match self {
			Self::U => 0,
			Self::D => 1,
			Self::L => 2,
			Self::R => 3,
			Self::T => 4,
			Self::X => 5,
			Self::Q => 6,
			Self::O => 7,
		}
	}
	const fn mask(self) -> u8 {
		1 << self.index()
	}
}

#[derive(Clone, Copy, Default)]
pub struct ButtonSet(u8);

impl ButtonSet {
	const UP_LEFT: Self = Self(Button::U.mask() | Button::L.mask());
	const DOWN_LEFT: Self = Self(Button::D.mask() | Button::L.mask());
	const UP_RIGHT: Self = Self(Button::U.mask() | Button::R.mask());
	const DOWN_RIGHT: Self = Self(Button::D.mask() | Button::R.mask());
	pub fn has(self, b: Button) -> bool {
		(self.0 & b.mask()) != 0
	}
	pub fn add(&mut self, b: Button) {
		self.0 |= b.mask();
	}
	pub fn just(button: Button) -> Self {
		Self::from_iter([button])
	}
}

impl FromIterator<Button> for ButtonSet {
	fn from_iter<I: IntoIterator<Item = Button>>(it: I) -> Self {
		let mut set = Self::default();
		for x in it {
			set.add(x);
		}
		set
	}
}

impl MovieWriter {
	pub fn new(filename: Option<&str>) -> anyhow::Result<Self> {
		let (out, log) = if let Some(filename) = filename {
			let output_file = io::BufWriter::new(
				File::create(filename).context("error creating output file: {e}")?,
			);
			//println!("{word_list:?}");
			let mut zip = zip::ZipWriter::new(output_file);
			zip.start_file("Comments.txt", Default::default())?;
			zip.start_file("Subtitles.txt", Default::default())?;
			zip.start_file("Header.txt", Default::default())?;
			io::copy(
				&mut File::open("Header.txt").context("couldn't open Header.txt")?,
				&mut zip,
			)
			.context("error copying Header.txt to zip")?;
			zip.start_file("SyncSettings.json", Default::default())?;
			io::copy(
				&mut File::open("SyncSettings.json").context("couldn't open SyncSettings.json")?,
				&mut zip,
			)
			.context("error copying SyncSettings.json to zip")?;
			zip.start_file("Input Log.txt", Default::default())?;
			zip.write_all("[Input]
		LogKey:#Disk Index|Power|Reset|Open Tray|Close Tray|#P1 Left Stick Up / Down|P1 Left Stick Left / Right|P1 Right Stick Up / Down|P1 Right Stick Left / Right|P1 D-Pad Up|P1 D-Pad Down|P1 D-Pad Left|P1 D-Pad Right|P1 Select|P1 Start|P1 △|P1 X|P1 □|P1 ○|P1 L1|P1 L2|P1 R1|P1 R2|P1 Left Stick, Button|P1 Right Stick, Button|P1 Analog|
		".as_bytes())?;
			fs::create_dir_all("logs").context("couldn't create directory logs")?;
			let log_filename = format!(
				"logs/log-{:010}.txt",
				std::time::SystemTime::now()
					.duration_since(std::time::UNIX_EPOCH)?
					.as_millis()
			);
			let log = File::create(&log_filename)
				.with_context(|| format!("couldn't create log file {log_filename}"))?;
			let log = io::BufWriter::new(log);
			(Some(zip), Some(log))
		} else {
			(None, None)
		};
		Ok(Self {
			out,
			finished: false,
			frames: 0,
			log,
		})
	}
	fn silent_clone(&self) -> Self {
		Self {
			out: None,
			log: None,
			frames: self.frames,
			finished: self.finished,
		}
	}
	fn write_frame(&mut self, buttons: ButtonSet) -> anyhow::Result<()> {
		assert!(!self.finished);
		let frames = self.frames;
		self.frames += 1;
		if self.out.is_none() && self.log.is_none() {
			return Ok(());
		}
		let mut s = *b"|    0,....|  128,  128,  128,  128,.................|\n";
		if buttons.has(Button::U) {
			s[36] = b'U';
		}
		if buttons.has(Button::D) {
			s[37] = b'D';
		}
		if buttons.has(Button::L) {
			s[38] = b'L';
		}
		if buttons.has(Button::R) {
			s[39] = b'R';
		}
		if buttons.has(Button::T) {
			s[42] = b'T';
		}
		if buttons.has(Button::X) {
			s[43] = b'X';
		}
		if buttons.has(Button::Q) {
			s[44] = b'Q';
		}
		if buttons.has(Button::O) {
			s[45] = b'O';
		}
		if let Some(out) = self.out.as_mut() {
			out.write_all(&s)?;
		}
		if false {
			// uses a lot of disk space but provides easy access to input log
			if let Some(log) = self.log.as_mut() {
				write!(log, "{frames:6}")?;
				log.write_all(&s)?;
			}
		}
		Ok(())
	}
	pub fn wait(&mut self, n: u32) -> anyhow::Result<()> {
		for _ in 0..n {
			self.write_frame(ButtonSet::default())?;
		}
		Ok(())
	}
	pub fn finish(&mut self) -> anyhow::Result<()> {
		if let Some(log) = self.log.as_mut() {
			writeln!(log, "[/Input]")?;
		}
		if let Some(out) = self.out.as_mut() {
			out.finish()?;
		}
		self.finished = true;
		Ok(())
	}
}

pub struct Game {
	rng: Rng,
	board: Board,
	rack: [Tile; 7],
	tile_bag: TileBag,
	word_list: Arc<WordList>,
	writer: MovieWriter,
	start_frame: u32,
	moves_played: u32,
	debug: bool,
}

const MUSIC_DELAY: u32 = 23;
const MUSIC_INTERVAL: u32 = 14430;
const LAYOUT_RAW: [(u8, u8, u8, u8); 17] = [
	(7, 7, 6, b'H'),
	(8, 5, 6, b'V'),
	(9, 5, 6, b'H'),
	(9, 9, 6, b'H'),
	(13, 8, 6, b'V'),
	(8, 13, 6, b'H'),
	(4, 11, 6, b'H'),
	(6, 8, 6, b'V'),
	(1, 14, 5, b'H'),
	(0, 8, 6, b'H'),
	(0, 9, 5, b'V'),
	(3, 4, 6, b'V'),
	(0, 6, 6, b'H'),
	(6, 0, 6, b'V'),
	(4, 3, 6, b'H'),
	(7, 1, 6, b'H'),
	(0, 0, 6, b'H'),
];
lazy_static::lazy_static! {
	static ref LAYOUT: [PlayStructure; 17] = LAYOUT_RAW.map(parse_play_structure);
}

impl Game {
	pub fn new(
		output_filename: Option<&str>,
		start_frame: u32,
		debug: bool,
		word_list: Arc<WordList>,
	) -> anyhow::Result<Self> {
		let mut game = Self {
			rng: Rng::default(),
			board: Board::default(),
			tile_bag: TileBag::default(),
			rack: [Tile::default(); 7],
			word_list,
			writer: MovieWriter::new(output_filename)?,
			start_frame,
			debug,
			moves_played: 0,
		};
		game.wait(1176)?;
		game.rng = Rng::default(); // rng starts here
		game.wait(676)?;
		// skip intro 1
		game.press(Button::X, 4)?;
		game.wait(40)?;
		// skip intro 2
		game.press(Button::X, 4)?;
		game.wait(932)?;
		// select play
		game.press(Button::X, 2)?;
		game.wait(2)?;
		// switch to solitaire mode
		game.press(Button::D, 2)?;
		game.wait(2)?;
		// select solitaire mode
		game.press(Button::X, 2)?;
		game.wait(2)?;
		game.wait_to(start_frame)?;
		// select default player name
		game.writer.write_frame(ButtonSet::just(Button::X))?;
		for _ in 0..5 {
			game.writer.write_frame(ButtonSet::default())?;
		}
		game.rng.skip(6);
		game.tile_bag.shuffle(&mut game.rng);
		game.tile_bag.fill_rack(&mut game.rng, &mut game.rack);
		game.wait(136)?;
		if start_frame % 2 == 1 {
			// may be just superstition
			game.wait(1)?;
		}
		Ok(game)
	}
	pub fn moves_played(&self) -> u32 {
		self.moves_played
	}
	fn wait_to(&mut self, frame: u32) -> anyhow::Result<()> {
		assert!(self.frames() <= frame);
		self.wait(frame - self.frames())
	}
	/// number of frames since power on
	pub fn frames(&self) -> u32 {
		self.writer.frames
	}

	fn silent_clone(&self) -> Self {
		Self {
			rng: self.rng.clone(),
			board: self.board.clone(),
			rack: self.rack,
			tile_bag: self.tile_bag.clone(),
			word_list: Arc::clone(&self.word_list),
			writer: self.writer.silent_clone(),
			start_frame: self.start_frame,
			moves_played: self.moves_played,
			debug: false,
		}
	}

	fn setup_play(&mut self, play: Play) -> anyhow::Result<()> {
		let mut board_cursor = Square::at(7, 7);
		let forward = play.forward();
		let mut board_target = play.start_square();
		let mut rack = self.rack;
		macro_rules! wait {
			($n:expr) => {
				self.wait($n)?
			};
		}
		macro_rules! press {
			($button:expr, $n:expr) => {
				self.press($button, $n)?
			};
		}
		macro_rules! press_set {
			($set:expr, $n:expr) => {
				self.press_set($set, $n)?
			};
		}
		for (i, letter) in play.iter_letters().enumerate() {
			let mut rack_cursor = rack.iter().position(|t| !t.is_empty()).expect("bad play");
			let rack_target = if play.is_blank(i as u8) {
				rack.iter().position(|t| t.is_blank())
			} else {
				rack.iter().position(|t| t.is_letter(letter))
			}
			.expect("bad play");
			// move rack cursor
			while rack_cursor != rack_target {
				press!(Button::R, 4);
				wait!(10);
				rack_cursor += 1;
				while rack[rack_cursor].is_empty() {
					rack_cursor += 1;
				}
			}
			rack[rack_target] = Tile::default();
			// select tile
			press!(Button::X, 4);
			wait!(2);
			// move it up onto the board
			press!(Button::U, 4);
			wait!(10);
			// move board cursor
			while board_cursor.y() < board_target.y() && board_cursor.x() < board_target.x() {
				press_set!(ButtonSet::DOWN_RIGHT, 4);
				wait!(12);
				board_cursor = board_cursor.below().unwrap().right().unwrap();
			}
			while board_cursor.y() < board_target.y() && board_cursor.x() > board_target.x() {
				press_set!(ButtonSet::DOWN_LEFT, 4);
				wait!(12);
				board_cursor = board_cursor.below().unwrap().left().unwrap();
			}
			while board_cursor.y() > board_target.y() && board_cursor.x() < board_target.x() {
				press_set!(ButtonSet::UP_RIGHT, 4);
				wait!(12);
				board_cursor = board_cursor.above().unwrap().right().unwrap();
			}
			while board_cursor.y() > board_target.y() && board_cursor.x() > board_target.x() {
				press_set!(ButtonSet::UP_LEFT, 4);
				wait!(12);
				board_cursor = board_cursor.above().unwrap().left().unwrap();
			}
			while board_cursor.y() < board_target.y() {
				press!(Button::D, 4);
				wait!(12);
				board_cursor = board_cursor.below().unwrap();
			}
			while board_cursor.x() < board_target.x() {
				press!(Button::R, 4);
				wait!(12);
				board_cursor = board_cursor.right().unwrap();
			}
			while board_cursor.y() > board_target.y() {
				press!(Button::U, 4);
				wait!(12);
				board_cursor = board_cursor.above().unwrap();
			}
			while board_cursor.x() > board_target.x() {
				press!(Button::L, 4);
				wait!(12);
				board_cursor = board_cursor.left().unwrap();
			}
			// plop down tile
			press!(Button::X, 4);
			wait!(6);
			if play.is_blank(i as u8) {
				// select blank value
				if letter > b'M' {
					for _ in 0..b'Z' - letter + 1 {
						press!(Button::L, 4);
						wait!(2);
					}
				} else {
					for _ in 0..letter - b'A' {
						press!(Button::R, 4);
						wait!(2);
					}
				}
				// confirm
				press!(Button::X, 4);
				wait!(4);
			}

			board_cursor = if i == 0 || play.is_horizontal() {
				board_cursor.right()
			} else {
				board_cursor.below()
			}
			.unwrap_or(board_cursor);
			if i as u8 + 1 < play.letters_played() {
				loop {
					board_target = forward(board_target).expect("bad play");
					if !self.board.is_occupied(board_target) {
						break;
					}
				}
			}
		}
		// move to "end turn"
		let mut rack_cursor = rack.iter().position(|t| !t.is_empty()).unwrap_or(7);
		while rack_cursor < 7 {
			press!(Button::R, 4);
			wait!(8);
			rack_cursor += 1;
			while rack.get(rack_cursor).is_some_and(|t| t.is_empty()) {
				rack_cursor += 1;
			}
		}
		Ok(())
	}
	pub fn make_play(&mut self, play: Play, delay: u32) -> anyhow::Result<()> {
		if self.debug {
			println!("start play @ {} : {play:?}", self.frames());
		}
		let draw = self.frames() + delay;
		self.setup_play(play)?;
		play.remove_tiles_from_rack(&mut self.rack);
		// wait for the right moment
		let delay_to_draw = self.play_delay_to_draw(play);
		if self.debug {
			println!("delay to draw: {delay_to_draw} frames");
		}
		self.wait_to(draw - delay_to_draw)?;
		// now!
		if self.debug {
			println!("play at frame {}", self.frames());
		}
		self.press(Button::X, 6)?;
		// wait for draw
		self.wait_to(draw)?;
		if self.debug {
			println!("draw at frame {draw}");
		}
		self.tile_bag.fill_rack(&mut self.rng, &mut self.rack);
		self.wait(self.play_delay_from_draw(play))?;
		self.board.make_play(play);
		self.moves_played += 1;
		Ok(())
	}
	/// does the first secondary word of play come before the mainline word?
	///
	/// not currently used but might be useful in the future.
	pub fn is_secondary_word_first(&self, play: Play) -> bool {
		let start_square = play.start_square();
		let Some(w) = self.board.secondary_word(play, start_square) else {
			return false;
		};
		if play.is_horizontal() {
			start_square.x() == w.start_square().x() && w.start_square().y() < start_square.y()
		} else {
			start_square.y() == w.start_square().y() && w.start_square().x() < start_square.x()
		}
	}
	fn play_setup_time(&self, play: Play) -> u32 {
		let mut silent = self.silent_clone();
		silent.setup_play(play).unwrap();
		silent.frames() - self.frames()
	}

	/// delay between "end turn" button and draw from tile bag in frames
	pub fn play_delay_to_draw(&self, play: Play) -> u32 {
		fn word_premium_count(w: PlayedWord) -> u32 {
			let mut count = 0;
			for i in 0..w.word().len() {
				if w.is_new_tile(i) && w.square_at(i).premium().is_word() {
					count += 1;
				}
			}
			count
		}
		// initial delay
		let mut frames: u32 = 6; // if self.moves_played == 0 { 5 } else { 6 };
		let mainline_word = self.board.mainline_word(play);
		let secondary_words: Vec<_> = self.board.secondary_words(play).collect();
		// mainline word
		frames +=
			u32::from(mainline_word.word().len()) * 66 + word_premium_count(mainline_word) * 68;
		// secondary word
		for w in secondary_words {
			frames += u32::from(w.word().len()) * 66 + word_premium_count(w) * 68;
		}
		if play.is_bingo() {
			frames += 66;
		}
		// delay from last cursor motion to draw is only 64/66 frames rather than 66/68.
		frames -= 2;
		frames
	}

	/// delay between tile bag draw and rack appearing on screen in frames
	pub fn play_delay_from_draw(&self, play: Play) -> u32 {
		let mut score = self.board.score_of(play);
		let mut total = 34;
		while score > 0 {
			if score <= 20 {
				score -= 1;
				total += 1;
			} else {
				score -= 10;
				total += 1;
			}
		}
		total * 2
	}

	fn play_is_in_layout(&self, play: Play) -> bool {
		if play.letters_played() < 5 || play.letters_played() > 6 {
			return false;
		}
		LAYOUT.into_iter().any(|p| p.compatible_with(play))
	}

	/// returns (play, number of frames to wait)
	pub fn best_play(&self) -> Option<(Play, u32)> {
		let mut plays =
			self.board
				.get_plays_with_layout(&self.word_list, self.rack, LAYOUT.as_ref());
		const PLAYS_TO_CONSIDER: usize = 10;
		const DELAYS_TO_CONSIDER: usize = 50;
		// don't consider too many plays
		plays.truncate(PLAYS_TO_CONSIDER);
		#[derive(Default, Clone, Copy)]
		struct PlayInfo {
			index: usize,
			delay: u32,
			valuation: i32,
		}
		let mut play_info = [PlayInfo::default(); PLAYS_TO_CONSIDER];
		let mut delay_valuations = [0i32; DELAYS_TO_CONSIDER];
		for (i, play) in plays.iter().copied().enumerate() {
			play_info[i].index = i;
			let setup_time = self.play_setup_time(play);
			let delay_to_draw = self.play_delay_to_draw(play);
			let mut min_delay = setup_time + delay_to_draw;
			let music = self.nearest_music_change(self.frames());
			if music >= self.frames() + setup_time && music < self.frames() + min_delay {
				min_delay = music - self.frames() + MUSIC_DELAY + delay_to_draw;
			}
			let mut rng = self.rng.clone();
			rng.skip(min_delay);
			let delay_interval = 2;
			let mut new_board = self.board.clone();
			new_board.make_play(play);
			let new_board = new_board;
			let get_delay = |d: usize| {
				// wacky garbage to get right number modulo 2
				let delay = min_delay + delay_interval * d as u32;
				let desired_modularity = 1; // not always right
							//((self.frames() - self.start_frame + delay - delay_to_draw) / MUSIC_INTERVAL + initial_modularity) % 2;
				if (self.frames() + delay - delay_to_draw
					+ u32::from(
						music >= self.frames() && music < self.frames() + delay - delay_to_draw,
					)) % 2 == 1 - desired_modularity
				{
					delay + 1
				} else {
					delay
				}
			};
			for (d, val) in delay_valuations.iter_mut().enumerate() {
				let mut new_rack = self.rack;
				let mut new_bag = self.tile_bag.clone();
				let mut new_rng = self.rng.clone();
				let delay = get_delay(d);
				new_rng.skip(delay);
				play.remove_tiles_from_rack(&mut new_rack);
				new_bag.fill_rack(&mut new_rng, &mut new_rack);
				*val = i32::MIN;
				for play in
					new_board.get_plays_with_layout(&self.word_list, new_rack, LAYOUT.as_ref())
				{
					if !self.play_is_in_layout(play) {
						continue;
					}
					let mut new_new_rack = new_rack;
					let mut new_new_bag = new_bag.clone();
					play.remove_tiles_from_rack(&mut new_new_rack);
					for tile in new_new_rack {
						new_new_bag.add(tile);
					}
					*val = std::cmp::max(*val, new_new_bag.valuation() - delay as i32);
				}
				rng.skip(delay_interval);
			}
			let best_d = (0..DELAYS_TO_CONSIDER)
				.max_by_key(|d| delay_valuations[*d])
				.unwrap();
			play_info[i].valuation = delay_valuations[best_d];
			play_info[i].delay = get_delay(best_d);
		}
		let info = play_info
			.iter()
			.take(plays.len())
			.max_by_key(|p| p.valuation)?;
		Some((plays[info.index], info.delay))
	}

	pub fn make_best_play(&mut self) -> anyhow::Result<Option<Play>> {
		let Some((play, delay)) = self.best_play() else {
			return Ok(None);
		};
		self.make_play(play, delay)?;
		Ok(Some(play))
	}
	pub fn press_set(&mut self, set: ButtonSet, on_time: u32) -> anyhow::Result<()> {
		for _ in 0..on_time {
			self.writer.write_frame(set)?;
		}
		self.rng.skip(on_time);
		Ok(())
	}
	pub fn press(&mut self, button: Button, on_time: u32) -> anyhow::Result<()> {
		self.press_set(ButtonSet::just(button), on_time)
	}
	pub fn nearest_music_change(&self, frames: u32) -> u32 {
		if frames < self.start_frame + 9000 {
			return frames + 1_000_000;
		}
		(frames - self.start_frame + MUSIC_INTERVAL / 2) / MUSIC_INTERVAL * MUSIC_INTERVAL
			+ self.start_frame
	}

	pub fn wait(&mut self, mut t: u32) -> anyhow::Result<()> {
		let music = self.nearest_music_change(self.frames());
		if music >= self.frames() && music < self.frames() + t {
			t += MUSIC_DELAY;
		}
		self.rng.skip(t);
		self.writer.wait(t)?;
		Ok(())
	}
	pub fn finish(&mut self) -> anyhow::Result<()> {
		self.writer.finish()
	}
	pub fn board(&self) -> &Board {
		&self.board
	}
	pub fn rack(&self) -> [Tile; 7] {
		self.rack
	}
	pub fn tile_bag(&self) -> &TileBag {
		&self.tile_bag
	}
}
