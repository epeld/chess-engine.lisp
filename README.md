# Chess Engine.lisp

This started out as a Common Lisp project but is now an emacs script that works with `emacs-chess` to support engine analysis (by using e.g Stockfish).

Screenie:
![alt sceenie](screenie.png)

## Usage:
Use `emacs-chess` to read a chess game using `chess-pgn-read`.

Once you have the board up and running, load `analysis.el` then type `chess-analysis-bindings` to install some key bindings.

Now you can get a quick analysis of every position by using the `n` and `p` keys to navigate back and forth. There is also `C-c C-e` which prompts you for some args to send to "go". Type in "infinite" and hit enter and the engine begins to search. Type `C-c C-g` when you are done to stop the engine again.

There should be a buffer `Chess Analysis Overview` which will always contain up to date info about what the engine is doing.