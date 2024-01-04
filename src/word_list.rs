use crate::{play::Word, tile::Tile};
use anyhow::Context;
use std::{
	borrow::Cow,
	collections::{HashMap, HashSet},
	fmt,
	fs::File,
	io::{prelude::*, BufReader},
};

#[derive(PartialEq, Eq, Hash, Clone, Copy, Default)]
pub struct Letters {
	counts: [u8; 26],
}

impl FromIterator<u8> for Letters {
	fn from_iter<T: IntoIterator<Item = u8>>(iter: T) -> Self {
		let mut me = Self::default();
		for l in iter {
			me.add(l);
		}
		me
	}
}

impl Letters {
	pub fn add(&mut self, c: u8) {
		assert!(c.is_ascii_uppercase());
		self.counts[usize::from(c - b'A')] += 1;
	}
	pub fn from_bytes(bytes: &[u8]) -> Self {
		Self::from_iter(bytes.iter().copied())
	}
	pub fn from_word(word: &Word) -> Self {
		Self::from_bytes(word.as_bytes())
	}
}

pub struct WordList {
	words: Vec<Word>,
	prefixes: HashSet<Word>,
	word_set: HashSet<Word>,
	by_letters: HashMap<Letters, Vec<usize>>,
}

impl fmt::Debug for WordList {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{:?}", self.words)
	}
}

impl WordList {
	fn from_words(words: Vec<Word>) -> Self {
		let mut by_letters: HashMap<Letters, Vec<usize>> = HashMap::new();
		for (i, word) in words.iter().enumerate() {
			by_letters
				.entry(Letters::from_word(word))
				.or_default()
				.push(i);
		}
		let mut prefixes = HashSet::new();
		for word in words.iter() {
			for i in 0..=word.len() {
				prefixes.insert(word.prefix(i));
			}
		}
		Self {
			word_set: words.iter().copied().collect(),
			words,
			prefixes,
			by_letters,
		}
	}
	pub fn words_with_letters(&self, letters: Letters) -> impl '_ + Iterator<Item = Word> {
		static EMPTY: Vec<usize> = Vec::new();
		self.by_letters
			.get(&letters)
			.unwrap_or(&EMPTY)
			.iter()
			.map(|i| self.words[*i])
	}
	pub fn bingos_for_tiles(&self, tiles: &[Tile]) -> Cow<'_, [usize]> {
		let mut letters = Letters::default();
		let mut blank_count = 0;
		for tile in tiles.iter() {
			if tile.is_empty() {
				return Cow::Borrowed(&[]);
			} else if tile.is_blank() {
				blank_count += 1;
			} else {
				letters.add(tile.as_byte());
			}
		}
		let mut temp_rack = [Tile::default(); 15];
		if blank_count == 0 {
			return Cow::Borrowed(self.by_letters.get(&letters).map(|v| &v[..]).unwrap_or(&[]));
		}
		let mut i = 0;
		for tile in tiles.iter() {
			if !tile.is_blank() {
				temp_rack[i] = *tile;
				i += 1;
			}
		}
		let mut bingos = vec![];
		for c in b'A'..=b'Z' {
			temp_rack[i] = Tile::from_byte(c);
			bingos.extend_from_slice(&self.bingos_for_tiles(&temp_rack[..tiles.len()]));
		}
		bingos.into()
	}
	pub fn is_prefix_of_word(&self, prefix: Word) -> bool {
		self.prefixes.contains(&prefix)
	}
	pub fn is_word(&self, word: Word) -> bool {
		self.word_set.contains(&word)
	}
}

pub fn read_word_list() -> anyhow::Result<WordList> {
	let filename = "scrabble-psx-word-list.txt";
	let file = File::open(filename).with_context(|| format!("error opening {filename}"))?;
	let mut list = vec![];
	for line in BufReader::new(file).lines() {
		let line = line.with_context(|| format!("error reading {filename}"))?;
		if line.is_empty() {
			continue;
		}
		let words = &line[..line.find(" \\ ").unwrap()];
		for word in words.split(' ') {
			list.push(word.to_uppercase().into());
		}
	}
	Ok(WordList::from_words(list))
}
