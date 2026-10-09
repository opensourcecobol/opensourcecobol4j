## Fixed

- **Fix `MOVE` from group items to `PIC N` items** (#910)
  - In older versions, moving a group item to a `PIC N` item converted the half-width characters in the group item (including half-width spaces) into full-width characters.
  - A group item is now moved to a `PIC N` item byte by byte, without any conversion, like other group moves. If the source is shorter than the receiving item, the remaining bytes are filled with half-width spaces. `JUSTIFIED RIGHT` is supported.
