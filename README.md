<meta charset="utf-8"/>

# `stc-s` or `STCSLibRust`

Library to read/write IVOA STC-S strings in Rust, and to convert them from/to JSON.
The parser has been implemented following:
* [the STC-S working note](https://ivoa.net/documents/STC-S/20130917/WD-STC-S-1.0-20130917.html);
* with modifications supporting [section 6 of the TAP standard](https://ivoa.net/documents/TAP/20100327/REC-TAP-1.0.html).

Other existing implementations:
* pure python: see the [gavo/dachs/sts](https://gitlab-p4n.aip.de/gavo/dachs/-/tree/main/gavo/stc)
* *Help me to fill this!*

## Motivations

This library has been first developped to be used in [MOC lib Rust](https://github.com/cds-astro/cds-moc-rust) to support
the creation of MOCs -- S-MOCs but why not ST-MOCs, F-MOCs, ... -- from STC−Strings.
This feature has been requested in [MOCPy](https://github.com/cds-astro/mocpy), 
see [issue #111](https://github.com/cds-astro/mocpy/issues/111).

It may also be used in:
* QAT2S positional queries 
* [Aladin Lite V3](https://github.com/cds-astro/aladin-lite) to display regions and 
  to replace a [minimal JS](https://github.com/cds-astro/aladin-lite/blob/develop/src/js/Overlay.js#L83) code 
  so far only supporting polygons and circles is already existing.
* Astropy Region, see [this issue](https://github.com/astropy/regions/issues/21)
    + the existing [pure python parser by Markus Demleitner](https://gitlab-p4n.aip.de/gavo/dachs/-/tree/main/gavo/stc) could be use?


## Status

Although this library is recent, and still has to be tested thoroughly,
it is already in production in [MOC Lib Rust](https://github.com/cds-astro/cds-moc-rust) 
to create S-MOCs from a STC-Strings.
See examples in the [MOCCli README](https://github.com/cds-astro/cds-moc-rust/tree/main/crates/cli).


## STC-S String from API example

```rust
Stc::new().set_space(
  PositionInterval::from_frame(Frame::ICRS)
    .set_refpos(SpaceTimeRefPos::Geocenter)
    .set_lo_hi_limits(vec![170.0, -20.0, 190.0, 10.0])
    .set_resolution(vec![0.0001])
).to_string();
```
gives the string:
```txt
"PositionInterval ICRS GEOCENTER 170 -20 190 10 Resolution 0.0001"
```
You can parse the string back to an object using (`unwrap` must be avoided, see tests in `lib.rs` 
for a complete example):
```rust
let ascii = "PositionInterval ICRS GEOCENTER 170 -20 190 10 Resolution 0.0001";
let stc = Stc::parse::<VerboseError<&str>>(ascii).unwrap().1;
```

## STC-S to JSON conversion examples

The following examples are extracted from the internal tests.

### Example 1

```text
Circle ICRS TOPOCENTER 147.6 69.9 0.4
```
is converted into:
```json
{
  "space": {
    "Circle": {
      "frame": "ICRS",
      "refpos": "TOPOCENTER",
      "pos": [
        147.6,
        69.9
      ],
      "radius": 0.4
    }
  }
}
```

### Example 2

```text
Time TDB BARYCENTER MJD 50814.0 
Position ICRS BARYCENTER 147.3 69.3
```
is converted into:
```json
{
  "time": {
    "Time": {
      "timescale": "TDB",
      "refpos": "BARYCENTER",
      "time": {
        "MJD": "50814.0"
      }
    }
  },
  "space": {
    "frame": "ICRS",
    "refpos": "BARYCENTER",
    "pos": [
      147.3,
      69.3
    ]
  }
}
```

### Example 3

```text
TimeInterval TT GEOCENTER 1996-01-01T00:00:00 1996-01-01T00:30:00
 Time MJD 50814.0 Error 1.2 Resolution 0.8 PixSize 1024.0
Circle ICRS GEOCENTER 179.0 -11.5 0.5 Position 179.0 -11.5
 Error 0.000889 Resolution 0.001778 Size 0.000333 0.000278
 PixSize 0.000083 0.000083
Spectral BARYCENTER 1420.4 unit MHz Resolution 10.0
RedshiftInterval BARYCENTER VELOCITY OPTICAL 200.0 2300.0
 Redshift 300.0 Resolution 0.7 PixSize 0.3
```
is converted into:
```json
{
  "time": {
    "TimeInterval": {
      "timescale": "TT",
      "refpos": "GEOCENTER",
      "start": [
        {
          "Iso": "1996-01-01T00:00:00Z"
        }
      ],
      "stop": [
        {
          "Iso": "1996-01-01T00:30:00Z"
        }
      ],
      "time": {
        "MJD": "50814.0"
      },
      "error": 1.2,
      "resolution": 0.8,
      "pixsize": 1024.0
    }
  },
  "space": {
    "Circle": {
      "frame": "ICRS",
      "refpos": "GEOCENTER",
      "pos": [
        179.0,
        -11.5
      ],
      "radius": 0.5,
      "position": [
        179.0,
        -11.5
      ],
      "error": [
        0.000889
      ],
      "resolution": [
        0.001778
      ],
      "size": [
        0.000333,
        0.000278
      ],
      "pixsize": [
        0.000083,
        0.000083
      ]
    }
  },
  "spectral": {
    "Value": {
      "refpos": "BARYCENTER",
      "value": 1420.4,
      "unit": "MHz",
      "resolution": 10.0
    }
  },
  "redshift": {
    "RedshiftInterval": {
      "refpos": "BARYCENTER",
      "type": "VELOCITY",
      "dopplerdef": "OPTICAL",
      "lolimit": [
        200.0
      ],
      "hilimit": [
        2300.0
      ],
      "redshift": 300.0,
      "resolution": 0.7,
      "pixsize": 0.3
    }
  }
}
```


### Example 4

```text
Union ICRS TOPOCENTER
(Circle 180 10 20
Circle 190 20 20
Intersection
 (Circle 120 -10 20
  Difference
   (Circle 130 -10 20
    Circle 125 -10 2
   )
  Not
   (Circle 118 -8 3)
 )
)
```
is converted into:
```json
{
  "space": {
    "Union": {
      "frame": "ICRS",
      "refpos": "TOPOCENTER",
      "elems": [
        {
          "Circle": {
            "pos": [
              180.0,
              10.0
            ],
            "radius": 20.0
          }
        },
        {
          "Circle": {
            "pos": [
              190.0,
              20.0
            ],
            "radius": 20.0
          }
        },
        {
          "Intersection": {
            "elems": [
              {
                "Circle": {
                  "pos": [
                    120.0,
                    -10.0
                  ],
                  "radius": 20.0
                }
              },
              {
                "Difference": {
                  "left": {
                    "Circle": {
                      "pos": [
                        130.0,
                        -10.0
                      ],
                      "radius": 20.0
                    }
                  },
                  "right": {
                    "Circle": {
                      "pos": [
                        125.0,
                        -10.0
                      ],
                      "radius": 2.0
                    }
                  }
                }
              },
              {
                "Not": {
                  "Circle": {
                    "pos": [
                      118.0,
                      -8.0
                    ],
                    "radius": 3.0
                  }
                }
              }
            ]
          }
        }
      ]
    }
  }
}
```

## To-do list

* [X] Parse and write STC-S.
* [X] Support JSON serialization and deserialization.
* [X] Support parsing of STC-S as defined in TAP.
* [ ] Make a CLI.
* [ ] Make a JS/Wasm library.
* [X] Add everywhere builders, getters and setters like in the `FillFrameRefposFlavor` structure to make a clean API.
* [X] Create a visitor.
    + [X] implement an `empty` visitor
    + [X] implement an `echo` visitor
    + [X] implement a `stcs2moc` visitor (available in [MOCLibRust](https://github.com/cds-astro/cds-moc-rust))
    + [ ] implement a visitor building a 'contains' method testing if a given point is inside the region or not
* [ ] Implement `fold` to avoid too wide lines.
* [ ] Support for STC XML serialization/deserialization?


## License

Like most projects in Rust, this project is licensed under either of

* Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or
  http://www.apache.org/licenses/LICENSE-2.0)
* MIT license ([LICENSE-MIT](LICENSE-MIT) or
  http://opensource.org/licenses/MIT)

at your option.


## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this project by you, as defined in the Apache-2.0 license,
shall be dual licensed as above, without any additional terms or conditions.
