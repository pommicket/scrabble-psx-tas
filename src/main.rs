#[allow(unused_imports)]
use rayon::prelude::*;
use std::process::ExitCode;

pub mod board;
pub mod game;
pub mod play;
pub mod tile;
pub mod word_list;
use std::sync::{Mutex, Arc};

fn try_main() -> anyhow::Result<()> {
	static COUNT: Mutex<usize> = Mutex::new(0);
	let word_list = Arc::new(crate::word_list::read_word_list()?);
	for century in 0.. {
		println!("---- century {century} ----");
		let check_play = |delay: &u32| {
			let delay = *delay;
			let mut game = game::Game::new(None, delay, false, Arc::clone(&word_list)).unwrap();
			while game.make_best_play().unwrap().is_some() {}
			if game.rack().iter().any(|t| !t.is_empty()) {
				// bad
				let mut c = COUNT.lock().unwrap();
				*c += 1;
				if game.tile_bag().count() == 0 {
					println!(
						"[{c}] delay {delay} fails after {} moves with rack {:?} tile bag {:?}",
						game.moves_played(),
						game.rack(),
						game.tile_bag(),
					);
				}
				return false;
			}
			let time = game.frames();
			println!("delay {delay} takes {} frames", time);
			true
		};
		let best_delay = (3_000u32 + 10_000 * century..13_000u32 + 10_000 * century)
			.into_par_iter()
			.find_any(check_play);
		let Some(best_delay) = best_delay else {
			continue;
		};
		let mut game = game::Game::new(Some(&format!("out-{century:06}.bk2")), best_delay, true, Arc::clone(&word_list))?;

		while let Some(play) = game.make_best_play()? {
			println!("make play {play:?}");
			println!("{:?}", game.board());
			println!("rack: {:?}", game.rack());
			println!("bag: {:?}", game.tile_bag());
		}
		game.finish()?;
		println!("--- best delay is {best_delay} ---");
	}
	Ok(())
}

fn main() -> ExitCode {
	match try_main() {
		Ok(()) => ExitCode::SUCCESS,
		Err(e) => {
			eprintln!("{e}");
			ExitCode::FAILURE
		}
	}
}
