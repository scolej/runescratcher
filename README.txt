Rune Scribbler
==============


Overview
--------

!! work-in-progress !!

This is the beginnings of a proof-of-concept of a game which sees you
explore a world which you can transform around you by writing runes onto
the environment. For example, a particular rune might mirror everything
within its area-of-effect vertically. Another might rotate everything by 90
degrees. The player must navigate the environment solely by writing &
erasing runes in the right places at the right times.


Keys
----

'up' 'down' 'left' 'right'

Just what you'd expect: moves the player around in the world.

'w' and then 'a' 's' 'd' or 'f' and then 'up' 'down' 'left' or 'right'

Select and write a rune in the given direction.


The runes
---------

Currently there's only one, and it mirrors the world vertically about the
rune's position.



How can I play this theoretical game of yours?
------------------------------------------------

- Check out the repo
- Install Guile (3.0.2 seems to be working)
- If you're feeling lucky, run the tests: `./test.sh`
- Start the game: `./run.sh`
- Wander the map and see how far you can tunnel into a wall by placing
  vertical mirror runes, eg: w s up
