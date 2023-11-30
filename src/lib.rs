use std::{
  fmt::Write,
  fmt::{self, Display},
};

use serde::{Deserialize, Serialize};

use nom::{
  character::complete::multispace0,
  combinator::{map, opt},
  error::{FromExternalError, ParseError},
  sequence::{preceded, tuple},
  IResult,
};

pub mod common;
pub mod redshift;
pub mod space;
pub mod spectral;
pub mod time;

use self::{redshift::Redshift, space::Space, spectral::Spectral, time::Time};

pub trait NomErr<'a>: ParseError<&'a str> + FromExternalError<&'a str, String> {}

impl<'a, T> NomErr<'a> for T where T: ParseError<&'a str> + FromExternalError<&'a str, String> {}

/// STC-S phrase.
#[derive(Serialize, Deserialize, Default, Debug, PartialEq)]
pub struct Stc {
  #[serde(skip_serializing_if = "Option::is_none")]
  time: Option<Time>,
  #[serde(skip_serializing_if = "Option::is_none")]
  space: Option<Space>,
  #[serde(skip_serializing_if = "Option::is_none")]
  spectral: Option<Spectral>,
  #[serde(skip_serializing_if = "Option::is_none")]
  redshift: Option<Redshift>,
}
impl Stc {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn set_time(mut self, time: Time) -> Self {
    self.set_time_by_ref(time);
    self
  }
  pub fn set_time_by_ref(&mut self, time: Time) {
    self.time.replace(time);
  }

  pub fn set_space(mut self, space: Space) -> Self {
    self.set_space_by_ref(space);
    self
  }
  pub fn set_space_by_ref(&mut self, space: Space) {
    self.space.replace(space);
  }

  pub fn set_spectral(mut self, spectral: Spectral) -> Self {
    self.set_spectral_by_ref(spectral);
    self
  }
  pub fn set_spectral_by_ref(&mut self, spectral: Spectral) {
    self.spectral.replace(spectral);
  }

  pub fn set_redshift(mut self, redshift: Redshift) -> Self {
    self.set_redshift_by_ref(redshift);
    self
  }
  pub fn set_redshift_by_ref(&mut self, redshift: Redshift) {
    self.redshift.replace(redshift);
  }

  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      tuple((
        opt(preceded(multispace0, Time::parse::<E>)),
        opt(preceded(multispace0, Space::parse::<E>)),
        opt(preceded(multispace0, Spectral::parse::<E>)),
        opt(preceded(multispace0, Redshift::parse::<E>)),
      )),
      |(time, space, spectral, redshift)| Self {
        time,
        space,
        spectral,
        redshift,
      },
    )(input)
  }
}
impl Display for Stc {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut first_sub_phrase = true;
    if let Some(time) = &self.time {
      Display::fmt(time, f)?;
      first_sub_phrase = false
    }
    if let Some(space) = &self.space {
      if first_sub_phrase {
        first_sub_phrase = false;
      } else {
        f.write_char('\n')?;
      }
      Display::fmt(space, f)?;
    }
    if let Some(spectral) = &self.spectral {
      if first_sub_phrase {
        first_sub_phrase = false;
      } else {
        f.write_char('\n')?;
      }
      Display::fmt(spectral, f)?;
    }
    if let Some(redshift) = &self.redshift {
      if !first_sub_phrase {
        f.write_char('\n')?;
      }
      Display::fmt(redshift, f)?;
    }
    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use nom::{
    error::{convert_error, VerboseError},
    Err,
  };

  #[test]
  fn test_from_stcs_doc_1() {
    test(
      "Circle ICRS TOPOCENTER 147.6 69.9 0.4",
      r#"{
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
}"#,
      false,
    );
  }

  #[test]
  fn test_from_stcs_doc_2() {
    test(
      r#"Time TDB BARYCENTER MJD 50814.0 Position ICRS BARYCENTER 147.3 69.3"#,
      r#"{
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
}"#,
      false,
    );
  }
  #[test]
  fn test_from_stcs_doc_3() {
    test(
      r#"TimeInterval TT GEOCENTER 2011-01-01 2012-03-30 Resolution 0.2"#,
      r#"{
  "time": {
    "TimeInterval": {
      "timescale": "TT",
      "refpos": "GEOCENTER",
      "start": [
        {
          "Iso": "2011-01-01"
        }
      ],
      "stop": [
        {
          "Iso": "2012-03-30"
        }
      ],
      "resolution": 0.2
    }
  }
}"#,
      false,
    );
  }

  #[test]
  fn test_from_stcs_doc_4() {
    test(
      r#"TimeInterval TT GEOCENTER 2011-01-01 2012-03-30 Resolution 0.2
PositionInterval ICRS GEOCENTER 170 -20 190 10 Resolution 0.0001
SpectralInterval GEOCENTER 4.95 5 unit GHz Size 0.05"#,
      r#"{
  "time": {
    "TimeInterval": {
      "timescale": "TT",
      "refpos": "GEOCENTER",
      "start": [
        {
          "Iso": "2011-01-01"
        }
      ],
      "stop": [
        {
          "Iso": "2012-03-30"
        }
      ],
      "resolution": 0.2
    }
  },
  "space": {
    "frame": "ICRS",
    "refpos": "GEOCENTER",
    "lo_hi_limits": [
      170.0,
      -20.0,
      190.0,
      10.0
    ],
    "resolution": [
      0.0001
    ]
  },
  "spectral": {
    "Interval": {
      "refpos": "GEOCENTER",
      "lolimit": [
        4.95
      ],
      "hilimit": [
        5.0
      ],
      "unit": "GHz",
      "size": 0.05
    }
  }
}"#,
      false,
    );
  }

  #[test]
  fn test_from_stcs_doc_5() {
    test(
      r#"Union ICRS TOPOCENTER
( Polygon 147.8 69.2 147.4 69.2 147.3 69.4 147.9 69.4
Polygon 147.9 69.7 147.6 69.7 147.5 69.9 148 69.9 )"#,
      r#"{
  "space": {
    "Union": {
      "frame": "ICRS",
      "refpos": "TOPOCENTER",
      "elems": [
        {
          "Polygon": {
            "pos": [
              147.8,
              69.2,
              147.4,
              69.2,
              147.3,
              69.4,
              147.9,
              69.4
            ]
          }
        },
        {
          "Polygon": {
            "pos": [
              147.9,
              69.7,
              147.6,
              69.7,
              147.5,
              69.9,
              148.0,
              69.9
            ]
          }
        }
      ]
    }
  }
}"#,
      false,
    );
  }

  #[test]
  fn test_from_stcs_doc_6() {
    test(
      r#"Union ICRS TOPOCENTER
( Circle 180 10 20
Circle 190 20 20
Intersection
( Circle 120 -10 20
Difference
( Circle 130 -10 20
Circle 125 -10 2
)
Not
( Circle 118 -8 3 )
)
)"#,
      r#"{
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
}"#,
      false,
    );
  }

  #[test]
  fn test_from_stcs_doc_7() {
    test(
      r#"TimeInterval TT GEOCENTER 1996-01-01T00:00:00Z 1996-01-01T00:30:00Z
 Time MJD 50814.0 Error 1.2 Resolution 0.8 PixSize 1024
Circle ICRS GEOCENTER 179 -11.5 0.5 Position 179 -11.5
 Error 0.000889 Resolution 0.001778 Size 0.000333 0.000278
 PixSize 0.000083 0.000083
Spectral BARYCENTER 1420.4 unit MHz Resolution 10
RedshiftInterval BARYCENTER VELOCITY OPTICAL 200 2300
 Redshift 300 Resolution 0.7 PixSize 0.3"#,
      r#"{
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
}"#,
      false,
    );
  }

  #[test]
  fn test_from_stcs_doc_8() {
    // PAS BON => SpectralInterval BARYCENTER Angstrom 4000 7000 Resolution 300 600"#,
    test(
      r#"StartTime TT BARYCENTER 1900-01-01
Circle ICRS BARYCENTER 148.9 69.1 2.0
 Resolution 0.0001 0.0001 0.0003 0.0003 Size 0.5 0.5 0.67 0.67
 PixSize 0.00005 0.00005 0.00015 0.00015
SpectralInterval BARYCENTER 4000 7000 unit Angstrom Resolution 300 600"#,
      r#"{
  "time": {
    "StartTime": {
      "timescale": "TT",
      "refpos": "BARYCENTER",
      "start": {
        "Iso": "1900-01-01"
      }
    }
  },
  "space": {
    "Circle": {
      "frame": "ICRS",
      "refpos": "BARYCENTER",
      "pos": [
        148.9,
        69.1
      ],
      "radius": 2.0,
      "resolution": [
        0.0001,
        0.0001,
        0.0003,
        0.0003
      ],
      "size": [
        0.5,
        0.5,
        0.67,
        0.67
      ],
      "pixsize": [
        0.00005,
        0.00005,
        0.00015,
        0.00015
      ]
    }
  },
  "spectral": {
    "Interval": {
      "refpos": "BARYCENTER",
      "lolimit": [
        4000.0
      ],
      "hilimit": [
        7000.0
      ],
      "unit": "Angstrom",
      "resolution": {
        "lo": 300.0,
        "hi": 600.0
      }
    }
  }
}"#,
      false,
    );
  }

  #[test]
  fn test_from_tap_section6_1() {
    test(
      r#"Circle ICRS GEOCENTER 10 20 0.5"#,
      r#"{
  "space": {
    "Circle": {
      "frame": "ICRS",
      "refpos": "GEOCENTER",
      "pos": [
        10.0,
        20.0
      ],
      "radius": 0.5
    }
  }
}"#,
      false,
    );
  }

  #[test]
  fn test_from_tap_section6_2() {
    test(
      r#"Position GALACTIC 10 20"#,
      r#"{
  "space": {
    "frame": "GALACTIC",
    "pos": [
      10.0,
      20.0
    ]
  }
}"#,
      false,
    );
  }

  #[test]
  fn test_from_tap_section6_3() {
    test(
      r#"Box CARTESIAN2 3 3 2 2"#,
      r#"{
  "space": {
    "Box": {
      "frame": "UNKNOWNFrame",
      "flavor": "CART2",
      "pos": [
        3.0,
        3.0
      ],
      "bsize": [
        2.0,
        2.0
      ]
    }
  }
}"#,
      false,
    );
  }

  #[test]
  fn test_from_tap_section6_4() {
    test(
      r#"Box 180 0 2 2"#,
      r#"{
  "space": {
    "Box": {
      "frame": "UNKNOWNFrame",
      "pos": [
        180.0,
        0.0
      ],
      "bsize": [
        2.0,
        2.0
      ]
    }
  }
}"#,
      false,
    );
  }

  #[test]
  fn test_from_tap_section6_5() {
    test(
      r#"Union ICRS ( Polygon 1 4 2 4 2 5 1 5 Polygon 3 4 4 4 4 5 3 5 )"#,
      r#"{
  "space": {
    "Union": {
      "frame": "ICRS",
      "elems": [
        {
          "Polygon": {
            "pos": [
              1.0,
              4.0,
              2.0,
              4.0,
              2.0,
              5.0,
              1.0,
              5.0
            ]
          }
        },
        {
          "Polygon": {
            "pos": [
              3.0,
              4.0,
              4.0,
              4.0,
              4.0,
              5.0,
              3.0,
              5.0
            ]
          }
        }
      ]
    }
  }
}"#,
      false,
    );
  }

  fn test(ascii_str: &str, json_str: &str, print_json: bool) {
    match Stc::parse::<VerboseError<&str>>(ascii_str) {
      Ok((rem, space)) => {
        // Ensures eveything is parsed
        assert_eq!(rem, "", "Remaining: {}", rem);
        // Compare re-serializing into STC-S string
        assert_eq!(
          space
            .to_string()
            .replace('\n', " ")
            .replace("  ", " ")
            .replace(".0 ", " ")
            .replace(" UNKNOWNFrame", ""),
          ascii_str
            .replace('\n', " ")
            .replace("  ", " ")
            .replace(".0 ", " ")
            .replace("CARTESIAN", "CART")
        );
        // Serialiaze in JSON
        let json = serde_json::to_string_pretty(&space).unwrap();
        if print_json {
          println!("Json:\n{}", json);
        }
        assert_eq!(json, json_str);
        // JSON round-trip to ensure JSON is well parsed
        let space2 = serde_json::from_str(&json).unwrap();
        assert_eq!(space, space2);
        // YAML: Not possible because: "untagged and internally tagged enums do not support enum input"
        /*if into_yaml {
          let yaml = serde_yaml::to_string(&space).unwrap();
          if print_yaml {
            println!("Yaml:\n{}", yaml);
          }
          let space3 = serde_yaml::from_str(&yaml).unwrap();
          assert_eq!(space, space3);
        }*/
      }
      Err(err) => {
        println!("Error: {:#?}", err);
        match err {
          Err::Incomplete(e) => println!("Error: {:?}", e),
          Err::Error(e) => println!("Error: {}", convert_error(ascii_str, e)),
          Err::Failure(e) => println!("Error: {}", convert_error(ascii_str, e)),
        };
        assert!(false)
      }
    }
  }
}
