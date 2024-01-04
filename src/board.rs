use crate::{
	play::{Play, PlayDirection, PlayStructure, Word},
	tile::{Tile, TileBag},
	word_list::{Letters, WordList},
};
use std::fmt;

#[derive(Clone)]
pub struct Board {
	squares: [u8; 15 * 15],
	blanks: [u16; 15],
}

impl Default for Board {
	fn default() -> Self {
		Self {
			squares: [b' '; 15 * 15],
			blanks: [0; 15],
		}
	}
}

impl fmt::Debug for Board {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		writeln!(f, "  ABCDEFGHIJKLMNO")?;
		for row in 0..15 {
			write!(f, "{:x} ", row + 1)?;
			for col in 0..15 {
				let letter = self.squares[15 * row + col];
				let c = if letter == b' ' {
					'.'
				} else {
					char::from(letter)
				};
				write!(f, "{c}")?;
			}
			writeln!(f)?;
		}
		Ok(())
	}
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Square(u8);

impl fmt::Debug for Square {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}{}", char::from(b'A' + self.0 % 15), 1 + self.0 / 15)
	}
}

impl Square {
	pub const fn new(i: u8) -> Self {
		assert!(i < 225);
		Self(i)
	}
	pub const fn x(self) -> u8 {
		self.0 % 15
	}
	pub const fn y(self) -> u8 {
		self.0 / 15
	}
	pub const fn at(x: u8, y: u8) -> Self {
		assert!(x < 15);
		assert!(y < 15);
		Self::new(y * 15 + x)
	}
	pub fn above(self) -> Option<Self> {
		assert!(self.0 < 15 * 15);
		if self.0 < 15 {
			None
		} else {
			Some(Self(self.0 - 15))
		}
	}
	pub fn below(self) -> Option<Self> {
		assert!(self.0 < 15 * 15);
		if self.0 + 15 >= 225 {
			None
		} else {
			Some(Self(self.0 + 15))
		}
	}
	pub fn left(self) -> Option<Self> {
		assert!(self.0 < 15 * 15);
		if self.0 % 15 == 0 {
			None
		} else {
			Some(Self(self.0 - 1))
		}
	}
	pub fn right(self) -> Option<Self> {
		assert!(self.0 < 15 * 15);
		if self.0 % 15 == 14 {
			None
		} else {
			Some(Self(self.0 + 1))
		}
	}
	pub fn premium(self) -> Premium {
		let mut x = self.0;
		if x > 112 {
			x = 224 - x;
		}
		match x {
			16 | 28 | 32 | 42 | 48 | 56 | 64 | 70 | 112 => Premium::DoubleWord,
			0 | 7 | 14 | 105 => Premium::TripleWord,
			3 | 11 | 36 | 38 | 45 | 52 | 59 | 92 | 96 | 98 | 102 => Premium::DoubleLetter,
			20 | 24 | 76 | 80 | 84 | 88 => Premium::TripleLetter,
			_ => Premium::None,
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Premium {
	None,
	DoubleLetter,
	TripleLetter,
	DoubleWord,
	TripleWord,
}

impl Premium {
	pub fn is_word(self) -> bool {
		matches!(self, Self::DoubleWord | Self::TripleWord)
	}
	pub fn is_letter(self) -> bool {
		matches!(self, Self::DoubleLetter | Self::TripleLetter)
	}
}

pub struct SecondaryWordIterator<'a> {
	board: &'a Board,
	play: Play,
	square: Option<Square>,
	i: usize,
}

impl<'a> SecondaryWordIterator<'a> {
	fn new(board: &'a Board, play: Play) -> Self {
		Self {
			board,
			play,
			square: Some(play.start_square()),
			i: 0,
		}
	}
}

impl Iterator for SecondaryWordIterator<'_> {
	type Item = PlayedWord;
	fn next(&mut self) -> Option<PlayedWord> {
		let start_square = self.square?;
		let letters = self.play.letters();
		if self.i >= letters.len() {
			self.square = None;
			return None;
		}
		let word = self.board.secondary_word(self.play, start_square);
		let forward = self.play.forward();
		self.square = self.square.and_then(forward);
		while self.square.is_some_and(|s| self.board.is_occupied(s)) {
			self.square = forward(self.square.unwrap());
		}
		self.i += 1;
		if let Some(word) = word {
			Some(word)
		} else {
			self.next()
		}
	}
}

impl Board {
	pub fn set(&mut self, square: Square, letter: u8) {
		assert!(letter == b' ' || letter.is_ascii_uppercase());
		self.squares[usize::from(square.0)] = letter;
	}
	pub fn get(&self, square: Square) -> u8 {
		self.squares[usize::from(square.0)]
	}
	pub fn make_play(&mut self, play: Play) {
		let mut square = play.start_square();
		let forward = play.forward();
		for (i, c) in play.iter_letters().enumerate() {
			while self.is_occupied(square) {
				square = forward(square).expect("bad play");
			}
			if c == b' ' {
				return;
			}
			if play.is_blank(i as u8) {
				self.blanks[usize::from(square.y())] |= 1 << square.x();
			}
			self.set(square, c);
		}
	}
	pub fn is_occupied(&self, square: Square) -> bool {
		self.get(square) != b' '
	}
	pub fn is_blank(&self, square: Square) -> bool {
		(self.blanks[usize::from(square.y())] & (1 << square.x())) != 0
	}

	pub fn mainline_word(&self, play: Play) -> PlayedWord {
		let mut sq = play.start_square();
		let forward = play.forward();
		let backward = play.backward();
		loop {
			let Some(b) = backward(sq) else {
				break;
			};
			if !self.is_occupied(b) {
				break;
			}
			sq = b;
		}
		let top = sq;
		let mut new_tiles: u16 = 0;
		let mut bit = 1;
		// letters preceding tiles played
		let mut word = Word::default();
		let mut blanks = 0;
		while self.is_occupied(sq) {
			if self.is_blank(sq) {
				blanks |= bit;
			}
			word.push(self.get(sq));
			sq = forward(sq).unwrap();
			bit <<= 1;
		}
		assert!(sq == play.start_square());
		let mut sq = Some(sq);
		for (i, letter) in play.iter_letters().enumerate() {
			word.push(letter);
			if play.is_blank(i as u8) {
				blanks |= bit;
			}
			sq = sq.and_then(forward);
			new_tiles |= bit;
			bit <<= 1;
			while let Some(s) = sq {
				if !self.is_occupied(s) {
					break;
				}
				word.push(self.get(s));
				sq = forward(s);
				bit <<= 1;
			}
		}
		if let Some(mut sq) = sq {
			// letters following tiles played
			while self.is_occupied(sq) {
				if self.is_blank(sq) {
					blanks |= bit;
				}
				word.push(self.get(sq));
				let Some(s) = forward(sq) else {
					break;
				};
				sq = s;
				bit <<= 1;
			}
		}
		debug_assert_eq!(new_tiles.count_ones(), play.letters_played().into());
		PlayedWord {
			word,
			new_tiles,
			direction: play.direction(),
			square: top,
			blanks,
		}
	}
	pub fn secondary_words(&self, play: Play) -> SecondaryWordIterator {
		SecondaryWordIterator::new(self, play)
	}
	pub fn secondary_word(&self, play: Play, square: Square) -> Option<PlayedWord> {
		assert!(!self.is_occupied(square));
		let mut top = square;
		let letter = play.letter_at(self, square)?;
		let letter_is_blank = play.is_blank_at(self, square)?;
		loop {
			if let Some(s) = ((!play.direction()).backward())(top) {
				if self.is_occupied(s) {
					top = s;
					continue;
				}
			}
			break;
		}
		let mut word = Word::default();
		let mut s = top;
		let mut new_tiles: u16 = 0;
		let mut blanks: u16 = 0;
		let mut i = 0;
		loop {
			let letter = if self.is_occupied(s) {
				self.get(s)
			} else {
				new_tiles |= 1 << i;
				letter
			};
			let is_blank = if self.is_occupied(s) {
				self.is_blank(s)
			} else {
				letter_is_blank
			};
			if is_blank {
				blanks |= 1 << i;
			}
			word.push(letter);
			let Some(next) = ((!play.direction()).forward())(s) else {
				break;
			};
			if next != square && !self.is_occupied(next) {
				break;
			}
			s = next;
			i += 1;
		}
		debug_assert!(new_tiles.count_ones() == 1);
		if word.len() > 1 {
			Some(PlayedWord {
				word,
				direction: !play.direction(),
				square: top,
				new_tiles,
				blanks,
			})
		} else {
			None
		}
	}

	pub fn is_legal(&self, word_list: &WordList, play: Play) -> bool {
		// make sure play is connected to existing tiles
		let mut sq = play.start_square();
		let mut connected = false;
		let forward = play.forward();
		if play.letters_played() > 7 {
			return false;
		}
		for i in 0..play.letters_played() {
			if i > 0 {
				let Some(s) = forward(sq) else {
					return false;
				};
				sq = s;
				while self.is_occupied(sq) {
					let Some(s) = forward(sq) else {
						return false;
					};
					sq = s;
				}
			}
			let f = |o: Option<Square>| o.is_some_and(|s| self.is_occupied(s));
			if f(sq.above()) || f(sq.below()) || f(sq.left()) || f(sq.right()) {
				connected = true;
				break;
			}
			if sq == Square::new(7 * 15 + 7) {
				connected = true;
				break;
			}
		}
		if !connected {
			return false;
		}
		// mainline word
		if !word_list.is_word(self.mainline_word(play).word) {
			return false;
		}

		// secondary words
		if self
			.secondary_words(play)
			.any(|p| !word_list.is_word(p.word))
		{
			return false;
		}
		true
	}

	#[allow(clippy::too_many_arguments)] // fuck you clippy
	fn get_plays_(
		&self,
		word_list: &WordList,
		rack: [Tile; 7],
		starting_square: Square,
		blanks: u8,
		curr_square: Square,
		curr_letters: Word,
		direction: PlayDirection,
		word_so_far: Word,
		plays: &mut Vec<Play>,
	) {
		debug_assert!(!self.is_occupied(curr_square));
		if !word_list.is_prefix_of_word(word_so_far) {
			return;
		}
		let forward = direction.forward();

		for i in 0..7 {
			let tile = rack[i];
			if tile.is_empty() {
				continue;
			}
			let all_options = b"ABCDEFGHIJKLMNOPQRSTUVWXYZ";
			let this = [tile.as_byte()];
			let options = if tile.is_blank() {
				all_options
			} else {
				&this[..]
			};
			let blanks = if tile.is_blank() {
				blanks | 1 << curr_letters.len()
			} else {
				blanks
			};
			let mut new_rack = rack;
			new_rack[i] = Tile::default();
			for letter in options.iter().copied() {
				let mut new_letters = curr_letters;
				new_letters.push(letter);
				let mut new_word = word_so_far;
				new_word.push(letter);
				let mut next_square = forward(curr_square);
				while let Some(sq) = next_square {
					if !self.is_occupied(sq) {
						break;
					}
					new_word.push(self.get(sq));
					next_square = forward(sq);
				}
				let play = Play::new(starting_square, direction, new_letters, blanks);
				if self
					.secondary_word(play, curr_square)
					.is_some_and(|w| !word_list.is_word(w.word))
				{
					// formed an invalid secondary word
					continue;
				}
				if word_list.is_word(new_word) && self.is_legal(word_list, play) {
					plays.push(play);
				}

				if let Some(sq) = next_square {
					self.get_plays_(
						word_list,
						new_rack,
						starting_square,
						blanks,
						sq,
						new_letters,
						direction,
						new_word,
						plays,
					);
				}
			}
		}
	}
	fn get_plays_with_structure(
		&self,
		word_list: &WordList,
		rack: [Tile; 7],
		structure: &PlayStructure,
		plays: &mut Vec<Play>,
	) {
		if self.is_occupied(structure.square) {
			return;
		}
		let mut square = structure.square;
		let forward = structure.direction.forward();
		let backward = structure.direction.backward();
		let mut letter_played_through = None;
		let mut letter_played_through_position = None;
		let mut is_connected = false;
		let mut square_after = None;
		for i in 0..structure.length {
			while self.is_occupied(square) {
				if letter_played_through.is_some() {
					// this structure is blocked
					return;
				}
				is_connected = true;
				letter_played_through = Some(self.get(square));
				letter_played_through_position = Some(i);
				match forward(square) {
					None => return,
					Some(s) => square = s,
				}
			}
			if square.above().is_some_and(|s| self.is_occupied(s))
				|| square.below().is_some_and(|s| self.is_occupied(s))
				|| square.left().is_some_and(|s| self.is_occupied(s))
				|| square.right().is_some_and(|s| self.is_occupied(s))
				|| square == Square::at(7, 7)
			{
				is_connected = true;
			}
			if i == structure.length - 1 {
				square_after = forward(square);
			}
			match forward(square) {
				None => {
					if i < structure.length - 1 {
						return;
					}
				}
				Some(s) => square = s,
			}
		}
		let letter_after = square_after.and_then(|s| {
			if self.is_occupied(s) {
				Some(self.get(s))
			} else {
				None
			}
		});
		if letter_after.is_some() && letter_played_through.is_some() {
			return;
		}
		let letter_before = backward(structure.square).and_then(|s| {
			if self.is_occupied(s) {
				Some(self.get(s))
			} else {
				None
			}
		});
		if letter_before.is_some() && letter_played_through.is_some() {
			return;
		}
		if letter_after.is_some() && letter_before.is_some() {
			return;
		}
		if letter_before.is_some() || letter_after.is_some() {
			is_connected = true;
		}
		if !is_connected {
			return;
		}
		let blanks = rack.into_iter().filter(|t| t.is_blank()).count();
		let blank_possibilities = 26u32.pow(blanks as u32);
		for blank_assignment in 0..blank_possibilities {
			let mut rack_letters = [0; 7];
			let mut blank_number = 0;
			let mut rack_letter = rack_letters.iter_mut();
			let mut rack_letter_count = 0;
			for tile in rack {
				if tile.is_empty() {
					continue;
				}
				rack_letter_count += 1;
				*rack_letter.next().unwrap() = if tile.is_blank() {
					blank_number += 1;
					b'A' + if blank_number == 1 {
						(blank_assignment % 26) as u8
					} else {
						(blank_assignment / 26) as u8
					}
				} else {
					tile.as_byte()
				};
			}

			for letter_map in 0..(1u8 << rack_letter_count) {
				if letter_map.count_ones() != structure.length.into() {
					continue;
				}
				let mut letter_set = Letters::default();
				for i in 0..7 {
					if (letter_map & (1 << i)) == 0 {
						continue;
					}
					assert!(rack_letters[i] != 0);
					letter_set.add(rack_letters[i]);
				}

				if let Some(letter) = letter_played_through {
					letter_set.add(letter);
				}
				if let Some(letter) = letter_before {
					letter_set.add(letter);
				}
				if let Some(letter) = letter_after {
					letter_set.add(letter);
				}
				for word in word_list.words_with_letters(letter_set) {
					let mut letters_played = word;
					if let (Some(letter), Some(position)) =
						(letter_played_through, letter_played_through_position)
					{
						if word[position.into()] != letter {
							continue;
						}
						letters_played.remove(position.into());
					} else if let Some(letter) = letter_before {
						if word[0] != letter {
							continue;
						}
						letters_played.remove(0);
					} else if let Some(letter) = letter_after {
						if word[word.len() as usize - 1] != letter {
							continue;
						}
						letters_played.remove(word.len() as usize - 1);
					}
					let mut blanks = 0;
					let mut rack_copy = rack;
					for (i, letter) in letters_played.as_bytes().iter().enumerate() {
						if let Some(tile) = rack_copy.iter_mut().find(|t| t.is_letter(*letter)) {
							*tile = Tile::EMPTY;
						} else {
							blanks |= 1 << i;
						}
					}
					let play = Play::new(
						structure.square,
						structure.direction,
						letters_played,
						blanks,
					);
					if self.is_legal(word_list, play) {
						plays.push(play);
					}
				}
			}
		}
	}
	pub fn get_plays_with_layout(
		&self,
		word_list: &WordList,
		rack: [Tile; 7],
		layout: &[PlayStructure],
	) -> Vec<Play> {
		let mut plays = vec![];
		for structure in layout {
			self.get_plays_with_structure(word_list, rack, structure, &mut plays);
		}
		plays.sort();
		plays.dedup();
		#[cfg(debug_assertions)]
		if !rack.iter().any(|t| t.is_blank()) {
			let mut check_plays = self.get_plays(word_list, rack);
			check_plays.retain(|p| layout.iter().any(|s| s.compatible_with(*p)));
			debug_assert_eq!(check_plays, plays);
		}
		plays
	}
	pub fn get_plays(&self, word_list: &WordList, rack: [Tile; 7]) -> Vec<Play> {
		let mut plays = vec![];
		for direction in [PlayDirection::Horizontal, PlayDirection::Vertical] {
			for s in 0..225 {
				let square = Square::new(s);
				if self.is_occupied(square) {
					continue;
				}
				let backward = direction.backward();
				let forward = direction.forward();
				// check letters preceding word
				let mut sq = square;
				loop {
					let Some(s) = backward(sq) else {
						break;
					};
					if !self.is_occupied(s) {
						break;
					}
					sq = s;
				}
				let mut can_connect = sq != square;
				let mut word_so_far = Word::default();
				while sq != square {
					assert!(self.is_occupied(sq));
					word_so_far.push(self.get(sq));
					sq = forward(sq).unwrap();
				}
				for _ in 0..7 {
					if sq.above().is_some_and(|s| self.is_occupied(s))
						|| sq.below().is_some_and(|s| self.is_occupied(s))
						|| sq.left().is_some_and(|s| self.is_occupied(s))
						|| sq.right().is_some_and(|s| self.is_occupied(s))
						|| sq == Square::at(7, 7)
					{
						can_connect = true;
					}
					if let Some(s) = forward(sq) {
						sq = s;
					} else {
						break;
					}
				}
				if !can_connect {
					continue;
				}
				self.get_plays_(
					word_list,
					rack,
					square,
					0,
					square,
					Word::default(),
					direction,
					word_so_far,
					&mut plays,
				);
			}
		}
		plays.sort();
		plays.dedup();
		plays
	}

	pub fn valuation(&self, rack: [Tile; 7], bag: &TileBag) -> i32 {
		let mut bag = bag.clone();
		// add rack tiles to bag for valuation
		for tile in rack {
			bag.add(tile);
		}
		// TODO: rack valuation
		bag.valuation()
	}

	fn score_of_word(&self, w: PlayedWord) -> u32 {
		let mut word_premium = 1;
		let mut score = 0;
		let word = w.word.as_bytes();
		for (i, c) in word.iter().copied().enumerate() {
			let sq = w.square_at(i as u8);
			let is_new = w.is_new_tile(i as u8);
			let blank = w.is_blank(i as u8);
			let premium = if is_new { sq.premium() } else { Premium::None };
			let mut letter_premium = 1;
			match premium {
				Premium::None => {}
				Premium::DoubleLetter => letter_premium = 2,
				Premium::TripleLetter => letter_premium = 3,
				Premium::DoubleWord => word_premium *= 2,
				Premium::TripleWord => word_premium *= 3,
			}
			score += letter_premium * if blank { 0 } else { Tile::from_byte(c).score() };
		}
		score * word_premium
	}
	pub fn score_of(&self, play: Play) -> u32 {
		let mut total = self.score_of_word(self.mainline_word(play));
		for w in self.secondary_words(play) {
			total += self.score_of_word(w);
		}
		total
	}
}

#[derive(Clone, Copy)]
pub struct PlayedWord {
	word: Word,
	direction: PlayDirection,
	square: Square,
	new_tiles: u16, // bitmask of which tiles were played
	blanks: u16,
}

impl fmt::Debug for PlayedWord {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{:?} from {:?}: ", self.direction, self.square)?;
		for (i, letter) in self.word.as_bytes().iter().enumerate() {
			let mut letter = *letter;
			if (self.new_tiles & (1 << i)) != 0 {
				letter = letter.to_ascii_uppercase();
			}
			write!(f, "{}", char::from(letter))?;
		}
		Ok(())
	}
}

impl PlayedWord {
	pub fn start_square(self) -> Square {
		self.square
	}
	pub fn square_at(self, i: u8) -> Square {
		assert!(i < self.word.len());
		Square::new(
			self.square.0
				+ match self.direction {
					PlayDirection::Horizontal => i,
					PlayDirection::Vertical => 15 * i,
				},
		)
	}
	pub fn is_new_tile(self, i: u8) -> bool {
		(self.new_tiles & (1 << i)) != 0
	}
	pub fn is_blank(self, i: u8) -> bool {
		(self.blanks & (1 << i)) != 0
	}
	pub fn word(self) -> Word {
		self.word
	}
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn præmia() {
		assert_eq!(Square::at(7, 7).premium(), Premium::DoubleWord);
		assert_eq!(Square::at(8, 6).premium(), Premium::DoubleLetter);
		assert_eq!(Square::at(6, 6).premium(), Premium::DoubleLetter);
		assert_eq!(Square::at(8, 8).premium(), Premium::DoubleLetter);
		assert_eq!(Square::at(0, 0).premium(), Premium::TripleWord);
		assert_eq!(Square::at(14, 14).premium(), Premium::TripleWord);
	}
}
