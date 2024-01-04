use crate::{
	board::{Board, Square},
	tile::Tile,
};
use std::fmt;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Word {
	len: u8,
	letters: [u8; 15],
}

impl std::ops::Index<usize> for Word {
	type Output = u8;
	fn index(&self, index: usize) -> &Self::Output {
		&self.letters[index]
	}
}

impl From<String> for Word {
	fn from(x: String) -> Self {
		assert!(x.len() <= 15);
		let mut word = Word::default();
		for letter in x.as_bytes().iter().copied() {
			word.push(letter);
		}
		word
	}
}

impl Default for Word {
	fn default() -> Self {
		Self {
			len: 0,
			letters: [b' '; 15],
		}
	}
}

impl fmt::Debug for Word {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(
			f,
			"{}",
			String::from_utf8(Vec::from(&self.letters[..usize::from(self.len)])).unwrap()
		)
	}
}

impl Word {
	#[allow(clippy::len_without_is_empty)]
	pub fn len(&self) -> u8 {
		self.len
	}
	pub fn prefix(&self, len: u8) -> Word {
		assert!(len <= self.len);
		let mut letters = [b' '; 15];
		letters[..usize::from(len)].copy_from_slice(&self.letters[..usize::from(len)]);
		Word { letters, len }
	}
	pub fn push(&mut self, c: u8) {
		assert!(self.len < 15);
		self.letters[usize::from(self.len)] = c;
		self.len += 1;
	}
	pub fn remove(&mut self, index: usize) {
		assert!(index < self.len.into());
		self.letters.copy_within(index + 1.., index);
		self.len -= 1;
		self.letters[usize::from(self.len)] = b' ';
	}
	pub fn as_bytes(&self) -> &[u8] {
		&self.letters[..usize::from(self.len)]
	}
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum PlayDirection {
	Horizontal,
	Vertical,
}

impl PlayDirection {
	pub fn forward(self) -> fn(Square) -> Option<Square> {
		match self {
			Self::Horizontal => Square::right,
			Self::Vertical => Square::below,
		}
	}
	pub fn backward(self) -> fn(Square) -> Option<Square> {
		match self {
			Self::Horizontal => Square::left,
			Self::Vertical => Square::above,
		}
	}
}

impl std::ops::Not for PlayDirection {
	type Output = Self;
	fn not(self) -> Self {
		match self {
			Self::Horizontal => Self::Vertical,
			Self::Vertical => Self::Horizontal,
		}
	}
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Play {
	square: Square,
	letters: Word,
	blanks: u8,
	direction: PlayDirection,
}

impl fmt::Debug for Play {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(
			f,
			"{:?} from {:?}: {:?} (blanks {:b})",
			self.direction, self.square, self.letters, self.blanks
		)
	}
}

impl Play {
	pub fn new(
		starting_square: Square,
		direction: PlayDirection,
		letters: Word,
		blanks: u8,
	) -> Self {
		Self {
			square: starting_square,
			direction,
			letters,
			blanks,
		}
	}
	pub fn start_square(self) -> Square {
		self.square
	}
	pub fn letters(&self) -> &[u8] {
		self.letters.as_bytes()
	}
	pub fn is_bingo(self) -> bool {
		self.letters_played() == 7
	}
	fn index_at(self, board: &Board, square: Square) -> Option<u8> {
		let mut s = self.start_square();
		let forward = self.forward();
		let mut i = 0;
		while s != square {
			if !board.is_occupied(s) {
				i += 1;
			}
			s = forward(s)?;
		}
		if board.is_occupied(s) {
			None
		} else {
			Some(i)
		}
	}
	pub fn is_blank_at(self, board: &Board, square: Square) -> Option<bool> {
		let i = self.index_at(board, square)?;
		Some(self.is_blank(i))
	}
	pub fn letter_at(self, board: &Board, square: Square) -> Option<u8> {
		let i = self.index_at(board, square)?;
		self.letters.as_bytes().get(usize::from(i)).copied()
	}
	pub fn iter_letters(&self) -> impl '_ + ExactSizeIterator<Item = u8> {
		self.letters.as_bytes().iter().copied()
	}
	pub fn forward(self) -> fn(Square) -> Option<Square> {
		self.direction().forward()
	}
	pub fn backward(self) -> fn(Square) -> Option<Square> {
		self.direction().backward()
	}
	pub fn letters_played(self) -> u8 {
		self.letters().len() as u8
	}
	pub fn direction(self) -> PlayDirection {
		self.direction
	}
	pub fn is_horizontal(self) -> bool {
		self.direction == PlayDirection::Horizontal
	}
	pub fn is_vertical(self) -> bool {
		self.direction == PlayDirection::Vertical
	}
	pub fn is_blank(self, index: u8) -> bool {
		(self.blanks & (1 << index)) != 0
	}
	pub fn remove_tiles_from_rack(self, rack: &mut [Tile; 7]) {
		for (i, letter) in self.letters.as_bytes().iter().copied().enumerate() {
			if (self.blanks & (1 << i)) != 0 {
				// remove blank
				*rack.iter_mut().find(|t| t.is_blank()).expect("bad play") = Tile::default();
			} else {
				*rack
					.iter_mut()
					.find(|t| t.is_letter(letter))
					.expect("bad play") = Tile::default();
			}
		}
	}
}

#[derive(Debug, Copy, Clone)]
pub struct PlayStructure {
	pub square: Square,
	pub direction: PlayDirection,
	pub length: u8,
}

impl PlayStructure {
	pub fn compatible_with(self, play: Play) -> bool {
		self.square == play.start_square()
			&& self.length == play.letters_played()
			&& self.direction == play.direction()
	}
}

pub const fn parse_play_structure(raw: (u8, u8, u8, u8)) -> PlayStructure {
	let (x, y, l, d) = raw;
	PlayStructure {
		square: Square::at(x, y),
		direction: match d {
			b'H' => PlayDirection::Horizontal,
			b'V' => PlayDirection::Vertical,
			_ => unreachable!(),
		},
		length: l,
	}
}
