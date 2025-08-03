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
- add itch page (ugh)
- menu as in a start button
- music (50%)

### only if time
- add fumes when accelerating
- add props
- add camera effects like zoom depending on speed or slight angling (= more zoomed in the start)
- wheels on both sides of a line should be prevented by adding collision to the connection from back to front wheel
- angle of arcs is based on connection between start and end, not actual angle
- apply effects like gradients to shadows or glow to platforms
- collectibles
- more path
- fix bug where rotational velocity is affected by user-input-affected wheel force
  (currently it over-rotates in the opposite direction of wheel grip)
  (tried to fix it, no dice)
