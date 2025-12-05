# Driver commands
A command to run the driver is given by the following grammar, with start symbol Command:

```
Command     → cabal run arrow Mode Environment Space Initial
Mode        → batch | interactive
Environment → filename
Space       → filename
Initial     → Pos Heading
Pos         → nat nat
Heading     → up | down | right | left
```

Here `filename` is a local directory reference and `nat` is any positive integer.

The example: cabal run arrow batch examples/SampleSpace.space examples/RemoveDebris.arrow 3 1 down
should run RemoveDebris.arrow using the batch driver on SampleSpace.space with starting position as (3,1), facing downwards.


# Driver output

## Batch driver

The output from the batch driver is defined by the following grammar, with start symbol Result:

```
Result  → Success | Fail
Success → final state: crlf Empty space crlf
Fail    → error: text
Empty   → crlf
```

Here `crlf` is a carriage return followed by line feed, represented in Haskell
as `"\r\n"`, and `text` is a string of characters.

Here `space` can be retrieved by printing the final state using printSpace in `Interpreter.hs`.

Example:
```
final state:

(7,7)
........
...v....
........
........
........
........
........
........

```

## Interactive driver

The output from the interactive driver is defined by the following grammar, with start symbol Result:

```
Result → Stage* Final | Stage* Fail
Stage  → space crlf Empty Input crlf
Input  → press enter to continue... crlf Empty
Final  → final state: crlf Empty space crlf
Fail   → error: text
Empty  → crlf
```

Here `crlf` is a carriage return followed by line feed, represented in Haskell
as `"\r\n"`, and `text` is a string of characters. Keep in mind that the empty line 
at the end of `Input` will be added by the user pressing enter.

Here `space` can be retrieved by printing the state using printSpace in `Interpreter.hs`.

Example:
```
(7,7)
........
........
........
........
......>.
........
........
........

press enter to continue...

(7,7)
........
........
........
........
.......>
........
........
........

press enter to continue...

final state:

(7,7)
........
........
........
........
.......>
........
........
........

```