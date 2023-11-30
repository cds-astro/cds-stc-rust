use std::fmt::{self, Display};

use nom::{
  bytes::complete::tag,
  character::complete::multispace1,
  combinator::{cut, map},
  multi::many1,
  number::complete::double,
  sequence::{preceded, tuple},
  IResult,
};
use serde::{Deserialize, Serialize};

use super::common::{FrameRefposFlavor, FromPosToVelocity};
use crate::NomErr;

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct Position {
  #[serde(flatten)]
  pub pre: FrameRefposFlavor,
  pub pos: Vec<f64>,
  #[serde(flatten)]
  pub post: FromPosToVelocity,
}
impl Position {
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      preceded(
        tag("Position"),
        tuple((
          cut(FrameRefposFlavor::parse),
          // pos
          cut(many1(preceded(multispace1, double))),
          cut(FromPosToVelocity::parse),
        )),
      ),
      |(pre, pos, post)| Self { pre, pos, post },
    )(input)
  }
}
impl Display for Position {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str("Position")?;
    self.pre.fmt(f)?;
    for pos in self.pos.iter() {
      f.write_fmt(format_args!(" {}", pos))?;
    }
    self.post.fmt(f)
  }
}
