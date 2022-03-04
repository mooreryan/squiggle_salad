Bad input

  $ pick_lines
  pick_lines: required arguments IDS, DATA are missing
  Usage: pick_lines [OPTION]… IDS DATA
  Try 'pick_lines --help' for more information.
  [1]
  $ pick_lines a
  pick_lines: required argument DATA is missing
  Usage: pick_lines [OPTION]… IDS DATA
  Try 'pick_lines --help' for more information.
  [1]
  $ pick_lines a b
  pick_lines: IDS argument: no 'a' file
  Usage: pick_lines [OPTION]… IDS DATA
  Try 'pick_lines --help' for more information.
  [1]
  $ pick_lines ids.txt b
  pick_lines: DATA argument: no 'b' file
  Usage: pick_lines [OPTION]… IDS DATA
  Try 'pick_lines --help' for more information.
  [1]
  $ pick_lines a data.txt
  pick_lines: IDS argument: no 'a' file
  Usage: pick_lines [OPTION]… IDS DATA
  Try 'pick_lines --help' for more information.
  [1]

Basic usage

  $ pick_lines ids.txt data.txt
  apple pie
  pie	stuff
  good   stuff
