> in development

# veloop
ride your motorbike on a line with loops and tricks and try not to fall.

Made for the [gmtk game jam 2025](https://itch.io/jam/gmtk-2025)

### running locally
After `npm install`, run with
```bash
npx elm-watch hot
```
and open the shown http link.

## TODO
- only/mainly take player input when last collision (ground contact) time
  was recent
- add itch page (ugh)
- never let the camera move below or above dead zones
- actual level
- menu as in a start button
- music
- sound effect, pitch/intensity directly mapped from wheel movement

### only if time
- add props
- add camera effects like zoom depending on speed or slight angling
- wheels on both sides of a line should be prevented by adding collision to the connection from back to front wheel
- angle of arcs is based on connection between start and end, not actual angle
- apply effects like gradients to shadows or glow to platforms
