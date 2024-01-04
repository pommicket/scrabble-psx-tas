use crate::game::Rng;
use std::fmt;

const BLANK: u8 = b'[';

#[derive(Clone, Copy)]
pub struct Tile(u8);
impl Default for Tile {
	fn default() -> Self {
		Self::from_byte(b' ')
	}
}

impl Tile {
	pub const EMPTY: Self = Self(0);
	pub fn from_byte(byte: u8) -> Self {
		assert!(byte.is_ascii_uppercase() || byte == b' ' || byte == BLANK);
		Self(byte)
	}
	pub fn as_byte(self) -> u8 {
		self.0
	}
	pub fn is_empty(self) -> bool {
		self.0 == b' '
	}
	pub fn is_blank(self) -> bool {
		self.0 == BLANK
	}
	pub fn score(self) -> u32 {
		match self.0 {
			b' ' | BLANK => 0,
			b'A' | b'E' | b'I' | b'L' | b'N' | b'O' | b'R' | b'S' | b'T' | b'U' => 1,
			b'D' | b'G' => 2,
			b'B' | b'C' | b'M' | b'P' => 3,
			b'F' | b'H' | b'V' | b'W' | b'Y' => 4,
			b'K' => 5,
			b'J' | b'X' => 8,
			b'Q' | b'Z' => 10,
			_ => unreachable!(),
		}
	}
	/// is this tile a vowel? blank is counted as both vowel and consonant.
	pub fn is_vowel(self) -> bool {
		matches!(self.0, b'A' | b'E' | b'I' | b'O' | b'U' | BLANK)
	}
	/// is this tile a consonant? blank is counted as both vowel and consonant.
	pub fn is_consonant(self) -> bool {
		!self.is_empty() && (!self.is_vowel() || self.is_blank())
	}
	/// is this tile exactly this letter? returns false if self is blank and letter is not [`BLANK`].
	pub fn is_letter(self, letter: u8) -> bool {
		self.0 == letter
	}
	pub fn valuation(self) -> i32 {
		// unscientific rough valuations of tiles
		4 * match self.0 {
			b' ' => 15,
			BLANK => 7,
			b'S' => 5,
			b'A' | b'E' | b'L' | b'N' | b'R' | b'T' => 3,
			b'O' | b'I' | b'D' => 2,
			b'B' | b'C' | b'M' | b'G' | b'P' => 1,
			b'U' | b'F' | b'H' => 0,
			b'K' | b'W' | b'Y' => -4,
			b'X' => -6,
			b'V' | b'Z' => -7,
			b'J' => -12,
			b'Q' => -20,
			_ => unreachable!("bad tile: {}", self.0),
		}
	}
}

impl fmt::Debug for Tile {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		if self.0 == BLANK {
			write!(f, "?")
		} else {
			write!(f, "{}", char::from(self.0))
		}
	}
}

#[derive(Clone)]
pub struct TileBag {
	tiles: [Tile; 100],
}

impl Default for TileBag {
	fn default() -> Self {
		let letter_counts: [u8; 27] = [
			9, 2, 2, 4, 12, 2, 3, 2, 9, 1, 1, 4, 2, 6, 8, 2, 1, 6, 4, 6, 4, 2, 2, 1, 2, 1, 2,
		];
		let mut tiles = [Tile::default(); 100];
		let mut tile_idx = 0;
		for i in 0..27 {
			for _ in 0..letter_counts[usize::from(i)] {
				tiles[tile_idx] = Tile::from_byte(b'A' + i);
				tile_idx += 1;
			}
		}
		Self { tiles }
	}
}

impl TileBag {
	pub fn shuffle(&mut self, rng: &mut Rng) {
		for _ in 0..300 {
			let r1 = rng.next();
			let r2 = rng.next();
			self.tiles.swap((r1 % 100) as usize, (r2 % 100) as usize);
		}
	}
	pub fn draw_one(&mut self, rng: &mut Rng) -> Option<Tile> {
		let count = self.tiles.into_iter().filter(|t| !t.is_empty()).count() as u32;
		if count == 0 {
			return None;
		}
		let r = rng.next() % count;
		let ptile = self
			.tiles
			.iter_mut()
			.filter(|t| !t.is_empty())
			.nth(r as usize)
			.unwrap();
		let tile = *ptile;
		*ptile = Tile::default();
		Some(tile)
	}
	/// draw tiles from bag to fill rack
	pub fn fill_rack(&mut self, rng: &mut Rng, rack: &mut [Tile; 7]) {
		for tile in rack.iter_mut().filter(|t| t.is_empty()) {
			let Some(t) = self.draw_one(rng) else {
				break;
			};
			*tile = t;
		}
	}
	/// number of vowels in tile bag. blank is counted as both vowel and consonant.
	pub fn vowels(&self) -> u8 {
		self.tiles.into_iter().filter(|t| t.is_vowel()).count() as u8
	}
	/// number of consonants in tile bag. blank is counted as both vowel and consonant.
	pub fn consonants(&self) -> u8 {
		self.tiles.into_iter().filter(|t| t.is_consonant()).count() as u8
	}
	pub fn count(&self) -> u8 {
		self.tiles.into_iter().filter(|t| !t.is_empty()).count() as u8
	}
	pub fn valuation(&self) -> i32 {
		let mut valuation = 0;
		for tile in self.tiles.into_iter() {
			valuation += tile.valuation();
		}
		let vowels = self.vowels();
		let consonants = self.consonants();
		let ratio = if vowels > consonants {
			(vowels as f32) / (consonants as f32 + 0.5)
		} else {
			(consonants as f32) / (vowels as f32 + 0.5)
		};
		valuation += i32::from(self.consonants()) - (32.0 * ratio) as i32;
		valuation
	}
	/// add tile back into bag
	pub fn add(&mut self, tile: Tile) {
		if tile.is_empty() {
			return;
		}
		*self
			.tiles
			.iter_mut()
			.find(|t| t.is_empty())
			.expect("TileBag::add() called on full bag") = tile;
	}
}

impl fmt::Debug for TileBag {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{{")?;
		for t in self.tiles {
			if !t.is_empty() {
				write!(f, "{t:?}")?;
			}
		}
		write!(f, "}}")?;
		Ok(())
	}
}

pub fn rack_valuation(rack: [Tile; 7]) -> i32 {
	let mut vowels = rack.iter().filter(|t| t.is_vowel()).count() as i32;
	if rack.iter().any(|t| t.is_blank()) {
		if vowels > 3 {
			vowels -= 1;
		} else {
			vowels += 1;
		}
	}
	4 * match vowels {
		0 => 2,
		1 => 3,
		2 => 6,
		3 => 10,
		4 => 6,
		5 => 2,
		6 => -2,
		7 => -5,
		_ => unreachable!(),
	}
}
