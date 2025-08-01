> in development

# sloope
ride your motorbike on a line with loops and tricks and try not to break your neck.

Made for the [gmtk game jam 2025](https://itch.io/jam/gmtk-2025)

### running locally
After `npm install`, run with
```bash
npx elm-watch hot
```
and open the shown http link.

## TODO
- if collides with multiple segments, divide combined force by collide count
- introduce/mainly use path segments that instead of colliding "snap" on intersection
  → then only/mainly take user input as how much to accelerate while snapped
  To figure out: interaction with only one wheel snapped = ?
  To figure out: when exactly to "unsnap"?
- add wheel movement (should directly map to user input + fade out for both wheels exactly the same rotation)
- add shadows
- add camera effects
- add props
- actual level
- menu as in a start button
- music
- sound effect, pitch/intensity directly mapped from wheel movement
