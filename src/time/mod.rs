use std::fmt::{self, Debug, Display};

use nom::{
  branch::alt,
  bytes::complete::tag,
  character::complete::multispace1,
  combinator::{map, opt},
  multi::many_m_n,
  sequence::{preceded, tuple},
  IResult,
};
use serde::{Deserialize, Serialize};

use crate::{time::common::DateTime, NomErr};

pub mod common;
use common::{FillfactorTimescaleRefpos, FromTimeToPixSize, FromUnitToPixSize, TimescaleRefpos};

/// Time sub-phrase
#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum Time {
  #[serde(rename = "TimeInterval")]
  TimeInterval(TimeElem<TimeIntervalArgs>),
  #[serde(rename = "StartTime")]
  StarTime(TimeElem<TimeStartArgs>),
  #[serde(rename = "StopTime")]
  StopTime(TimeElem<TimeStopArgs>),
  #[serde(rename = "Time")]
  Time(TimeSimple),
}
impl Display for Time {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self {
      Self::TimeInterval(t) => Display::fmt(t, f),
      Self::StarTime(t) => Display::fmt(t, f),
      Self::StopTime(t) => Display::fmt(t, f),
      Self::Time(t) => Display::fmt(t, f),
    }
  }
}
impl Time {
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    alt((
      map(TimeElem::<TimeIntervalArgs>::parse, Self::TimeInterval),
      map(TimeElem::<TimeStartArgs>::parse, Self::StarTime),
      map(TimeElem::<TimeStopArgs>::parse, Self::StopTime),
      map(TimeSimple::parse, Self::Time),
    ))(input)
  }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct TimeSimple {
  #[serde(flatten)]
  pub pre: TimescaleRefpos,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub time: Option<DateTime>,
  #[serde(flatten)]
  pub post: FromUnitToPixSize,
}
impl TimeSimple {
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      preceded(
        tag("Time"),
        tuple((
          TimescaleRefpos::parse,
          opt(preceded(multispace1, DateTime::parse)),
          FromUnitToPixSize::parse,
        )),
      ),
      |(pre, time, post)| Self { pre, time, post },
    )(input)
  }
}
impl Display for TimeSimple {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str("Time")?;
    Display::fmt(&self.pre, f)?;
    if let Some(time) = &self.time {
      f.write_fmt(format_args!(" {}", time))?;
    }
    Display::fmt(&self.post, f)
  }
}

/// A generic region, defined from its parameters type.
#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct TimeElem<T: TimeElemArgs> {
  #[serde(flatten)]
  pub pre: FillfactorTimescaleRefpos,
  #[serde(flatten)]
  pub params: T,
  #[serde(flatten)]
  pub post: FromTimeToPixSize,
}
impl<T: TimeElemArgs> TimeElem<T> {
  pub fn new(pre: FillfactorTimescaleRefpos, params: T, post: FromTimeToPixSize) -> Self {
    Self { pre, params, post }
  }
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      preceded(
        tag(T::ELEM_NAME),
        tuple((
          FillfactorTimescaleRefpos::parse,
          T::parse,
          FromTimeToPixSize::parse,
        )),
      ),
      |(pre, params, post)| Self { pre, params, post },
    )(input)
  }
}
impl<T: TimeElemArgs> Display for TimeElem<T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str(T::ELEM_NAME)?;
    Display::fmt(&self.pre, f)?;
    Display::fmt(&self.params, f)?;
    Display::fmt(&self.post, f)
  }
}

/// Trait defining a TimeElement from its name and its parameters parsing and display methods.
pub trait TimeElemArgs: Sized + Display {
  const ELEM_NAME: &'static str;
  fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E>;
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct TimeIntervalArgs {
  #[serde(skip_serializing_if = "Vec::is_empty")]
  pub start: Vec<DateTime>,
  #[serde(skip_serializing_if = "Vec::is_empty")]
  pub stop: Vec<DateTime>,
}
impl TimeElemArgs for TimeIntervalArgs {
  const ELEM_NAME: &'static str = "TimeInterval";
  fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      many_m_n(2, usize::MAX, preceded(multispace1, DateTime::parse)),
      |intervals| {
        let (start, stop): (Vec<(usize, DateTime)>, Vec<(usize, DateTime)>) = intervals
          .into_iter()
          .enumerate()
          .partition(|(i, _)| i % 2 == 0);
        let start = start.into_iter().map(|(_, v)| v).collect();
        let stop = stop.into_iter().map(|(_, v)| v).collect();
        Self { start, stop }
      },
    )(input)
  }
}
impl Display for TimeIntervalArgs {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for (start, stop) in self.start.iter().zip(self.stop.iter()) {
      f.write_fmt(format_args!(" {} {}", start, stop))?;
    }
    Ok(())
  }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct TimeStartArgs {
  pub start: DateTime,
}
impl TimeElemArgs for TimeStartArgs {
  const ELEM_NAME: &'static str = "StartTime";
  fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(preceded(multispace1, DateTime::parse), |start| Self {
      start,
    })(input)
  }
}
impl Display for TimeStartArgs {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_fmt(format_args!(" {}", &self.start))
  }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct TimeStopArgs {
  pub stop: DateTime,
}
impl TimeElemArgs for TimeStopArgs {
  const ELEM_NAME: &'static str = "StopTime";
  fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(preceded(multispace1, DateTime::parse), |stop| Self { stop })(input)
  }
}
impl Display for TimeStopArgs {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_fmt(format_args!(" {}", &self.stop))
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
  fn test_parse_simple() {
    let s = "StartTime TT BARYCENTER 1900-01-01";
    let json_str = r#"{
  "StartTime": {
    "timescale": "TT",
    "refpos": "BARYCENTER",
    "start": {
      "Iso": "1900-01-01"
    }
  }
}"#;
    match Time::parse::<VerboseError<&str>>(s) {
      Ok((rem, time)) => {
        assert_eq!(rem, "", "Remaining: {}", rem);
        // println!("Time: {}", time);
        // println!("Time: {:?}", time);
        assert_eq!(&time.to_string(), s);
        let json = serde_json::to_string_pretty(&time).unwrap();
        // println!("Json:\n{}", json);
        assert_eq!(json, json_str);
        /*let json_str = r#"{
          "StartTime": {
            "timescale": "TT",
            "refpos": "BARYCENTER",
            "start": {
              "Iso": "1900-501-01"
            }
          }
        }"#;
                         */
        match serde_json::from_str::<Time>(json_str) {
          Ok(time_fron_json) => {
            assert_eq!(time, time_fron_json)
          }
          Err(e) => {
            println!("{}", e.to_string());
            assert!(false)
          }
        }
      }
      Err(err) => {
        println!("Error: {:#?}", err);
        match err {
          Err::Incomplete(e) => println!("Error: {:?}", e),
          Err::Error(e) => println!("Error: {}", convert_error(s, e)),
          Err::Failure(e) => println!("Error: {}", convert_error(s, e)),
        };
        assert!(false)
      }
    }
  }
}
