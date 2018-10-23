val black_time = use_AI_file black.sml;
structure Black_AI = Reversi_AI;

val white_time = use_AI_file white.sml;
structure White_AI = Reversi_AI;

structure Game = Game_Engine(structure B = Black_AI structure W = White_AI);

Game.play black_time white_time;

OS.Process.exit OS.Process.success : unit;
