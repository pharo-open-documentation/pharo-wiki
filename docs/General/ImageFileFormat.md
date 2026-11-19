# Image file format
Pharo image is a binary file containing a snapshot of the runtime.
However, there is only a little documentation about its structure (how information is encoded as bytes).
This page is an early work to document the binary format of Pharo images.

- [Determinate the format of an image](#determinate-the-format-of-an-image)
- [Image format accross Pharo versions](#image-format-accross-pharo-versions)

## Determinate the format of an image
Each image has a format. The format can be retrieved by reading the 4 first bytes of an image file.
These bytes encode an integer from least significant to most significant byte (little-endian).

On MacOS, you can use `od` shell command to show these 4 bytes:

```bash
od -t x1 -N 4 Pharo.image
```

You will get an output as follow:

```
0000000    b5  09  01  00                                                
0000004
```

Which correspond to `68021` in decimal.

## Image format accross Pharo versions

| Pharo version                | Image format code | Image first 4 bytes (hex) |
|:-----------------------------|:------------------|:--------------------------|
| version <= 1.3               | 6504              | 68  19  00  00            |
| 1.3 < version <= 5.0-preSpur | 6505              | 69  19  00  00            |
| version (32bits) >= 5.0      | 6521              | 79  19  00  00            |
| version (64bits) >= 5.0      | 68021             | b5  09  01  00            |
