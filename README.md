> in development

# veloop
ride your motorbike on a line with loops and tricks and try not to break your neck.

Made for the [gmtk game jam 2025](https://itch.io/jam/gmtk-2025)

### running locally
After `npm install`, run with
```bash
npx elm-watch hot
```
and open the shown http link.

## TODO
- introduce/mainly use path segments that instead of colliding "snap" on intersection
  → then only/mainly take player input as how much to accelerate while snapped
  To figure out: interaction with only one wheel snapped = ?
  To figure out: when exactly to "unsnap"?
- add shadows
- add itch page (ugh)
- add camera effects
- add props
- actual level
- menu as in a start button
- music
- sound effect, pitch/intensity directly mapped from wheel movement
- make the motorbike a bit more pretty

too small bugs to probably matter
- wheels on both sides of a line should be prevented by adding collision to the connection from back to front wheel
- angle of arcs is based on connection between start and end, not actual angle
