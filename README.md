## PlayStation Scrabble (NTSC) TAS

this repo tries to generate a tool-assisted speedrun of Scrabble for the Sony PlayStation.

you will need the word list bundled with this game ( I am not distributing it to avoid DMCA takedown).
the file should look like this:

```
aa aas \ AA n pl. -S rough, cindery lava
aah aahs aahing aahed \ AAH v -ED, -ING, -S to exclaim in amazement, joy, or surprise
...
```

you can extract it from a ROM file (find it within the ROM file
by searching for the text above), although it is slightly annoying
because there are random garbage bytes interspersed at regular intervals
throughout the file (so you will need some script to get rid of those).
alternatively, you can e-mail me at pommicket at gmail dot com and I
will certainly refuse to provide you with the word list.

once you have the word list saved as `scrabble-psx-word-list.txt`, you can install Rust and do

```
cargo r --release
```

this will use all cores of your CPU at 100% and eventually produce files
called out-NNNNNN.bk2. these can be loaded into BizHawk.
currently they are not guaranteed to work and will almost certainly
get screwed up at some point (if it's close to the end of the game
you can do the rest manually though).
this is because input is only taken every other frame,
but not consistently always on odd frames or always on even frames (it can
even change halfway through a game) and I can't figure out the pattern…

code is currently kind of a mess but contributions are welcome.
