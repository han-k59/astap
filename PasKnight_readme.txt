================================================================================
PasKnight — a UCI chess engine
Version 1.0.1, homepage https://sourceforge.net/projects/pasknight/
================================================================================

--------------------------------------------------------------------------------
1. What it is
--------------------------------------------------------------------------------
PasKnight is a UCI (Universal Chess Interface) chess engine. It has no graphical
interface of its own; it runs as a console application and communicates through
text commands with a GUI such as Arena, Cute Chess, BanksiaGUI, or WinBoard/
XBoard (via a UCI-to-XBoard adapter).

Playing strength is about 2679 elo, see https://computerchess.org.uk/4040/index.html
It is a classical hand-crafted-evaluation (HCE) engine — it does NOT use a neural
network (NNUE). It runs on a single thread.

--------------------------------------------------------------------------------
2. Source code and building
--------------------------------------------------------------------------------
PasKnight is written in Object Pascal for the Free Pascal Compiler (FPC 3.2.2)
and the Lazarus IDE. It is a single-file program plus one include file for the
offline evaluation tuner.

  - Engine:  pasknight.lpr
  - Tuner:   pasknight_texel_tuner.pas  (compiled in only under -dTUNER)

Build the engine from the command line with:

    fpc -O3 pasknight.lpr

This produces the UCI executable. The tuner is a separate offline build enabled
by the TUNER define (see section 10); a normal engine build does not include it.

--------------------------------------------------------------------------------
3. Core loop
--------------------------------------------------------------------------------
PasKnight follows the standard UCI cycle:

  - Listens for UCI commands (uci, isready, ucinewgame, position, go, stop,
    setoption, quit, plus the extra display / bench / help / play commands).
  - Maintains its internal board state from position commands, supporting both
    "startpos" and full FEN strings, each optionally followed by a move list.
  - Searches for the best move on "go", honouring the time control or an
    explicit depth / movetime / nodes limit.
  - Streams search statistics ("info depth ... seldepth ... score ... nodes ...
    nps ... hashfull ... pv ...") and finally reports the chosen move with
    "bestmove".

--------------------------------------------------------------------------------
4. Board representation
--------------------------------------------------------------------------------
  - Bitboards (LERF layout): the board is a set of 64-bit integers, one per
    piece type per colour, with bit 0 = a1 (Little-Endian Rank-File mapping).
    A parallel 8x8 mailbox array is kept alongside for fast piece-type lookup
    on a square.
  - Magic bitboards: sliding-piece attacks (bishop, rook, queen) are generated
    by plain magic bitboard lookups — no ray-walking at runtime. Portable plain
    magics (no PEXT/BMI2) are used so the same code path runs on every target.
    The magic tables are re-verified constructively at startup.
  - Incremental state: king positions, castling rights, en-passant square, and
    the half-move (fifty-move) clock are tracked incrementally rather than
    recomputed. Search uses copy/restore of the game state (no separate
    make/unmake path), so all incremental fields roll back for free.

--------------------------------------------------------------------------------
5. Move generation
--------------------------------------------------------------------------------
  - Pseudo-legal generation: all moves valid by piece-movement rules are
    generated, temporarily ignoring whether the mover's king is left in check.
  - Legality filtering: a pseudo-legal move is played, the mover's king is
    tested for check, and the move is retracted if it is illegal.
  - Attack detection uses the same magic-bitboard machinery as move generation,
    so "is square attacked" queries are fast.

--------------------------------------------------------------------------------
6. Search algorithm
--------------------------------------------------------------------------------
The search is a Negamax alpha-beta with a modern selective-search feature set.

  - Iterative deepening: depth 1, 2, 3, ... The best move and principal
    variation from the previous iteration guide move ordering at the next, and
    the engine always has a move ready if time runs out (partial-depth recovery
    keeps the last completed depth on an abort).
  - Negamax + alpha-beta pruning: the single-perspective Minimax variant with
    alpha-beta cut-offs.
  - Principal Variation Search (PVS): the first move is searched with a full
    window, the rest with null windows and re-searched only on a fail-high.
  - Aspiration windows: deeper iterations start with a narrow window around the
    previous score, widening on fail-high/low.
  - Transposition-table cut-offs and move ordering (see sections 7 and 8).
  - Quiescence search: at the search horizon the engine keeps searching only
    captures, promotions, and check evasions (with delta pruning and SEE
    pruning) to avoid the horizon effect. Includes stand-pat and mate detection.
  - Selective search / pruning and reductions:
      * Null-move pruning, with a zugzwang guard (only when the side to move has
        non-pawn material) and a verification search; the reduction R scales
        with depth.
      * Reverse futility pruning (static null-move pruning), with an
        "improving"-conditioned margin.
      * Futility pruning at frontier nodes.
      * Late move reductions (LMR), adjusted by move history.
      * Late move pruning (LMP).
      * Razoring (verified, shallow non-PV nodes).
      * Internal iterative reductions (IIR) when there is no TT move.
      * Singular extensions for forced/critical TT moves.
      * SEE-based capture pruning in the main search (skips clearly losing
        captures at shallow non-PV nodes).
  - Extensions: check extensions add a ply when a move gives check, so tactical
    sequences are not cut off prematurely.
  - Static Exchange Evaluation (SEE): used both in quiescence and for
    main-search move ordering / capture pruning; computed lazily at the point a
    capture is actually selected, so each capture is evaluated at most once.

--------------------------------------------------------------------------------
7. Move ordering
--------------------------------------------------------------------------------
To maximise alpha-beta cut-offs, moves are tried best-first:

  - TT / PV move first: the best move from the transposition table (or the
    previous iteration's PV) is locked into the first slot.
  - MVV-LVA for captures: most-valuable-victim / least-valuable-attacker, so
    e.g. pawn-takes-queen is tried before queen-takes-pawn.
  - SEE bad-capture demotion: captures that lose material by static exchange are
    pushed below quiet moves.
  - Killer heuristic: quiet moves that produced a beta cut-off at the same ply
    in sibling nodes are prioritised.
  - History heuristic: a two-sided (bonus/malus) from-to history table, with a
    symmetric ordering clamp, refines the ordering of the remaining quiet moves.

--------------------------------------------------------------------------------
8. Caching and draw detection
--------------------------------------------------------------------------------
  - Zobrist hashing: the position is folded into a 64-bit key by XORing random
    per-piece-per-square constants, for fast position identity.
  - Transposition table: stores score, depth, best move, and bound type for
    previously searched positions. It is runtime-resizable through the UCI Hash
    option and uses a compact 16-byte packed entry (move in 19 bits, plus score,
    depth, and flag), giving high entry density and good cache behaviour.
  - Pawn hash table: the pawn-structure part of the evaluation depends only on
    the pawn placement, which repeats constantly during a search. A separate
    incremental pawn-only Zobrist key indexes a pawn hash that caches this work,
    so pawn-structure terms are recomputed only on a miss.
  - Repetition and draw detection: game-history hashes detect three-fold
    repetition; the half-move clock enforces the fifty-move rule; and there is
    insufficient-material detection.

--------------------------------------------------------------------------------
9. Evaluation function
--------------------------------------------------------------------------------
The evaluation is tapered between a middlegame (MG) and an endgame (EG) score,
interpolated by a game-phase value derived from the remaining non-pawn material.
Nearly all evaluation weights are Texel-tuned (fitted offline to game results;
see section 10). Terms include:

  - Material and piece-square tables: PeSTO-style tapered piece values and full
    MG/EG piece-square tables.
  - Pawn structure: passed pawns scored by rank (separate MG/EG tables);
    connected / phalanx pawns by rank; doubled, isolated, and backward-pawn
    penalties.
  - Piece activity: per-piece-type mobility tables indexed by the number of
    "safe" moves (squares not attacked by enemy pawns); knight and bishop
    outposts by rank; a "bad bishop" penalty for own pawns blocked on the
    bishop's own colour complex; a bishop-pair bonus.
  - Rooks: bonuses for rooks on open / semi-open files, on the 7th rank, and
    behind passed pawns (own or enemy).
  - King safety (middlegame): a nonlinear king-safety table driven by weighted
    counts of attackers around the king, plus a rank-indexed pawn-shield term.
  - King activity (endgame): king centralisation, and a king-to-passed-pawn
    distance term (the defending king wants to be near enemy passers, the
    attacking king near its own).
  - Threats: small bonuses for pawns attacking pieces, minors attacking heavy
    pieces, and hanging (attacked-and-undefended) pieces.

--------------------------------------------------------------------------------
10. Evaluation tuning (offline)
--------------------------------------------------------------------------------
The evaluation weights were fitted with an offline Texel tuner
(pasknight_texel_tuner.pas), a separate build enabled by the TUNER define. It
minimises the error between a sigmoid of the static evaluation and known game
results over large sets of labelled quiet positions (Zurichess quiet-labeled
and lichess-big3-resolved formats are both supported). It uses an Adam
optimiser with L2 anchoring, exactly reconstructs each evaluation term as a
linear combination of the tunable parameters (verified per position), and emits
the tuned parameter block as ready-to-paste Pascal. This tuner is only for
development; it is not part of the normal engine binary and is not needed to
play.

--------------------------------------------------------------------------------
11. Time management
--------------------------------------------------------------------------------
PasKnight uses a stability-based time-management scheme. It computes a per-move
allocation from the remaining clock, the increment, and the moves-to-go, then:

  - shortens the effective budget once the best move has been stable for several
    consecutive iterations (an "easy move" needs less time);
  - clamps the allocation between a control-relative minimum and a maximum that
    always reserves a safety margin, so the engine does not lose on time;
  - has an emergency mid-search abort ceiling for the rare case where a single
    deepening iteration runs far over budget, with partial-depth recovery.

--------------------------------------------------------------------------------
12. Opening book
--------------------------------------------------------------------------------
PasKnight can play from an opening book placed in the same directory as the
executable, using book moves while the position is still in book and switching
to search once it leaves book.

--------------------------------------------------------------------------------
13. UCI options and extra commands
--------------------------------------------------------------------------------
  - UCI option: Hash (transposition-table size in MB; the pawn hash is sized
    from the same budget).
  - Extra console commands beyond the UCI standard: "display" (ASCII board),
    "bench" (fixed benchmark over a set of positions, optionally to a given
    depth), "help", and "play" (a minimal text interface).

--------------------------------------------------------------------------------
14. Not included (by design, in the v1.00 / HCE phase)
--------------------------------------------------------------------------------
  - NNUE neural-network evaluation
  - Multi-threaded search (Lazy SMP)
  - Endgame tablebases (Syzygy)

These are potential directions beyond v1.00 but are intentionally out of scope
for the hand-crafted-evaluation release.
