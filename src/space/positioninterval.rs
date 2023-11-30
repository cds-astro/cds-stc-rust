use std::fmt::{self, Display};

use nom::{
  bytes::complete::tag,
  character::complete::multispace1,
  combinator::{cut, map},
  multi::many_m_n,
  number::complete::double,
  sequence::{preceded, tuple},
  IResult,
};
use serde::{Deserialize, Serialize};

use super::common::{FillFrameRefposFlavor, FromPosToVelocity};
use crate::NomErr;

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct PositionInterval {
  #[serde(flatten)]
  pub pre: FillFrameRefposFlavor,
  pub lo_hi_limits: Vec<f64>, // Vec<Range<f64>>
  #[serde(flatten)]
  pub post: FromPosToVelocity,
}
impl PositionInterval {
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      preceded(
        tag("PositionInterval"),
        tuple((
          cut(FillFrameRefposFlavor::parse),
          // pos and radius
          cut(many_m_n(2, usize::MAX, preceded(multispace1, double))),
          cut(FromPosToVelocity::parse),
        )),
      ),
      |(pre, lo_hi_limits, post)| Self {
        pre,
        lo_hi_limits,
        post,
      },
    )(input)
  }
}
impl Display for PositionInterval {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str("PositionInterval")?;
    self.pre.fmt(f)?;
    for elem in self.lo_hi_limits.iter() {
      f.write_fmt(format_args!(" {}", elem))?;
    }
    self.post.fmt(f)
  }
}

#[cfg(test)]
mod tests {
  use super::{
    super::common::{Frame, FrameRefposFlavor},
    *,
  };
  use crate::common::SpaceTimeRefPos as RefPos;
  use nom::error::VerboseError;

  #[test]
  fn test_position_interval() {
    let circle = PositionInterval {
      pre: FillFrameRefposFlavor {
        fillfactor: None,
        frame_refpos_flavor: FrameRefposFlavor {
          frame: Frame::ICRS,
          refpos: Some(RefPos::Geocenter),
          flavor: None,
        },
      },
      lo_hi_limits: vec![170.0, -20.0, 190.0, 10.0],
      post: FromPosToVelocity {
        position: None,
        unit: None,
        error: None,
        resolution: Some(vec![0.0001]),
        size: None,
        pixsize: None,
        velocity: None,
      },
    };

    let s = "PositionInterval ICRS GEOCENTER 170 -20 190 10 Resolution 0.0001";
    let (rem, pi) = PositionInterval::parse::<VerboseError<&str>>(s).unwrap();
    assert_eq!(circle, pi, "{}", rem);

    let json = serde_json::to_string_pretty(&circle).unwrap();
    // println!("{}", json);
    assert_eq!(
      json,
      r#"{
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
}"#
    );
  }
}
