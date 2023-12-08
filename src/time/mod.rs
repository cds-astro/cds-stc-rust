use std::fmt::{self, Debug, Display};

use nom::{
  branch::alt,
  bytes::complete::tag_no_case,
  character::complete::multispace1,
  combinator::{map, opt},
  multi::many_m_n,
  sequence::{preceded, tuple},
  IResult,
};
use serde::{Deserialize, Serialize};

use crate::{common::SpaceTimeRefPos, visitor::TimeVisitor, NomErr};

pub mod common;
use crate::common::ValOrRange;
use common::{
  DateTime, FillfactorTimescaleRefpos, FromTimeToPixSize, FromUnitToPixSize, TimeScale, TimeUnit,
  TimescaleRefpos,
};

/// Time sub-phrase
#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum Time {
  #[serde(rename = "Time")]
  Time(TimeSimple),
  #[serde(rename = "StartTime")]
  StarTime(TimeElem<TimeStartArgs>),
  #[serde(rename = "StopTime")]
  StopTime(TimeElem<TimeStopArgs>),
  #[serde(rename = "TimeInterval")]
  TimeInterval(TimeElem<TimeIntervalArgs>),
}
impl Time {
  pub fn accept<V: TimeVisitor>(&self, visitor: V) -> Result<V::Value, V::Error> {
    match self {
      Self::Time(t) => visitor.visit_time_simple(t),
      Self::StarTime(t) => visitor.visit_time_start(t),
      Self::StopTime(t) => visitor.visit_time_stop(t),
      Self::TimeInterval(t) => visitor.visit_time_interval(t),
    }
  }

  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    alt((
      map(TimeElem::<TimeIntervalArgs>::parse, Self::TimeInterval),
      map(TimeElem::<TimeStartArgs>::parse, Self::StarTime),
      map(TimeElem::<TimeStopArgs>::parse, Self::StopTime),
      map(TimeSimple::parse, Self::Time),
    ))(input)
  }
}
impl Display for Time {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self {
      Self::Time(t) => Display::fmt(t, f),
      Self::StarTime(t) => Display::fmt(t, f),
      Self::StopTime(t) => Display::fmt(t, f),
      Self::TimeInterval(t) => Display::fmt(t, f),
    }
  }
}
impl From<TimeSimple> for Time {
  fn from(value: TimeSimple) -> Self {
    Self::Time(value)
  }
}
impl From<TimeElem<TimeStartArgs>> for Time {
  fn from(value: TimeElem<TimeStartArgs>) -> Self {
    Self::StarTime(value)
  }
}
impl From<TimeElem<TimeStopArgs>> for Time {
  fn from(value: TimeElem<TimeStopArgs>) -> Self {
    Self::StopTime(value)
  }
}
impl From<TimeElem<TimeIntervalArgs>> for Time {
  fn from(value: TimeElem<TimeIntervalArgs>) -> Self {
    Self::TimeInterval(value)
  }
}

#[derive(Serialize, Deserialize, Default, Debug, PartialEq)]
pub struct TimeSimple {
  #[serde(flatten)]
  pre: TimescaleRefpos,
  #[serde(skip_serializing_if = "Option::is_none")]
  time: Option<DateTime>,
  #[serde(flatten)]
  post: FromUnitToPixSize,
}
impl TimeSimple {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn set_timescale(mut self, timescale: TimeScale) -> Self {
    self.set_timescale_by_ref(timescale);
    self
  }
  pub fn set_refpos(mut self, refpos: SpaceTimeRefPos) -> Self {
    self.set_refpos_by_ref(refpos);
    self
  }
  pub fn set_time(mut self, datetime: DateTime) -> Self {
    self.set_time_by_ref(datetime);
    self
  }
  pub fn set_unit(mut self, unit: TimeUnit) -> Self {
    self.set_unit_by_ref(unit);
    self
  }
  pub fn set_error(mut self, error: ValOrRange) -> Self {
    self.set_error_by_ref(error);
    self
  }
  pub fn set_resolution(mut self, resolution: ValOrRange) -> Self {
    self.set_resolution_by_ref(resolution);
    self
  }
  pub fn set_size(mut self, size: ValOrRange) -> Self {
    self.set_size_by_ref(size);
    self
  }
  pub fn set_pixsize(mut self, pixsize: ValOrRange) -> Self {
    self.set_pixsize_by_ref(pixsize);
    self
  }

  pub fn set_timescale_by_ref(&mut self, timescale: TimeScale) {
    self.pre.set_timescale_by_ref(timescale);
  }
  pub fn set_refpos_by_ref(&mut self, refpos: SpaceTimeRefPos) {
    self.pre.set_refpos_by_ref(refpos);
  }
  pub fn set_time_by_ref(&mut self, datetime: DateTime) {
    self.time.replace(datetime);
  }
  pub fn set_unit_by_ref(&mut self, unit: TimeUnit) {
    self.post.set_unit_by_ref(unit);
  }
  pub fn set_error_by_ref(&mut self, error: ValOrRange) {
    self.post.set_error_by_ref(error);
  }
  pub fn set_resolution_by_ref(&mut self, resolution: ValOrRange) {
    self.post.set_resolution_by_ref(resolution);
  }
  pub fn set_size_by_ref(&mut self, size: ValOrRange) {
    self.post.set_size_by_ref(size);
  }
  pub fn set_pixsize_by_ref(&mut self, pixsize: ValOrRange) {
    self.post.set_pixsize_by_ref(pixsize);
  }

  pub fn timescale(&self) -> Option<TimeScale> {
    self.pre.timescale()
  }
  pub fn timescale_or_default(&self) -> TimeScale {
    self.pre.timescale_or_default()
  }
  pub fn refpos(&self) -> Option<SpaceTimeRefPos> {
    self.pre.refpos()
  }
  pub fn refpos_or_default(&self) -> SpaceTimeRefPos {
    self.pre.refpos_or_default()
  }
  pub fn time(&self) -> Option<&DateTime> {
    self.time.as_ref()
  }
  pub fn unit(&self) -> Option<TimeUnit> {
    self.post.unit()
  }
  pub fn unit_or_default(&self) -> TimeUnit {
    self.post.unit_or_default()
  }
  pub fn error(&self) -> Option<&ValOrRange> {
    self.post.error()
  }
  pub fn resolution(&self) -> Option<&ValOrRange> {
    self.post.resolution()
  }
  pub fn size(&self) -> Option<&ValOrRange> {
    self.post.size()
  }
  pub fn pixsize(&self) -> Option<&ValOrRange> {
    self.post.pixsize()
  }

  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      preceded(
        tag_no_case("Time"),
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

/// A generic time element, defined from its parameters type.
#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct TimeElem<T: TimeElemArgs> {
  #[serde(flatten)]
  pre: FillfactorTimescaleRefpos,
  #[serde(flatten)]
  params: T,
  #[serde(flatten)]
  post: FromTimeToPixSize,
}
impl<T: TimeElemArgs> TimeElem<T> {
  pub(crate) fn new(pre: FillfactorTimescaleRefpos, params: T, post: FromTimeToPixSize) -> Self {
    Self { pre, params, post }
  }
  pub fn from_params(params: T) -> Self {
    Self::new(
      FillfactorTimescaleRefpos::default(),
      params,
      FromTimeToPixSize::default(),
    )
  }
  pub fn set_fillfactor(mut self, fillfactor: f64) -> Self {
    self.set_fillfactor_by_ref(fillfactor);
    self
  }
  pub fn set_timescale(mut self, timescale: TimeScale) -> Self {
    self.set_timescale_by_ref(timescale);
    self
  }
  pub fn set_refpos(mut self, refpos: SpaceTimeRefPos) -> Self {
    self.set_refpos_by_ref(refpos);
    self
  }
  pub fn set_time(mut self, time: DateTime) -> Self {
    self.set_time_by_ref(time);
    self
  }
  pub fn set_unit(mut self, unit: TimeUnit) -> Self {
    self.set_unit_by_ref(unit);
    self
  }
  pub fn set_error(mut self, error: ValOrRange) -> Self {
    self.set_error_by_ref(error);
    self
  }
  pub fn set_resolution(mut self, resolution: ValOrRange) -> Self {
    self.set_resolution_by_ref(resolution);
    self
  }
  pub fn set_size(mut self, size: ValOrRange) -> Self {
    self.set_size_by_ref(size);
    self
  }
  pub fn set_pixsize(mut self, pixsize: ValOrRange) -> Self {
    self.set_pixsize_by_ref(pixsize);
    self
  }

  pub fn set_fillfactor_by_ref(&mut self, fillfactor: f64) {
    self.pre.set_fillfactor_by_ref(fillfactor);
  }
  pub fn set_timescale_by_ref(&mut self, timescale: TimeScale) {
    self.pre.set_timescale_by_ref(timescale);
  }
  pub fn set_refpos_by_ref(&mut self, refpos: SpaceTimeRefPos) {
    self.pre.set_refpos_by_ref(refpos);
  }
  pub fn set_time_by_ref(&mut self, time: DateTime) {
    self.post.set_time_by_ref(time);
  }
  pub fn set_unit_by_ref(&mut self, unit: TimeUnit) {
    self.post.set_unit_by_ref(unit);
  }
  pub fn set_error_by_ref(&mut self, error: ValOrRange) {
    self.post.set_error_by_ref(error);
  }
  pub fn set_resolution_by_ref(&mut self, resolution: ValOrRange) {
    self.post.set_resolution_by_ref(resolution);
  }
  pub fn set_size_by_ref(&mut self, size: ValOrRange) {
    self.post.set_size_by_ref(size);
  }
  pub fn set_pixsize_by_ref(&mut self, pixsize: ValOrRange) {
    self.post.set_pixsize_by_ref(pixsize);
  }

  pub fn fillfactor(&self) -> Option<f64> {
    self.pre.fillfactor()
  }
  pub fn fillfactor_or_default(&self) -> f64 {
    self.pre.fillfactor_or_default()
  }
  pub fn timescale(&self) -> Option<TimeScale> {
    self.pre.timescale()
  }
  pub fn timescale_or_default(&self) -> TimeScale {
    self.pre.timescale_or_default()
  }
  pub fn refpos(&self) -> Option<SpaceTimeRefPos> {
    self.pre.refpos()
  }
  pub fn refpos_or_default(&self) -> SpaceTimeRefPos {
    self.pre.refpos_or_default()
  }
  pub fn time(&self) -> Option<&DateTime> {
    self.post.time()
  }
  pub fn unit(&self) -> Option<TimeUnit> {
    self.post.unit()
  }
  pub fn unit_or_default(&self) -> TimeUnit {
    self.post.unit_or_default()
  }
  pub fn error(&self) -> Option<&ValOrRange> {
    self.post.error()
  }
  pub fn resolution(&self) -> Option<&ValOrRange> {
    self.post.resolution()
  }
  pub fn size(&self) -> Option<&ValOrRange> {
    self.post.size()
  }
  pub fn pixsize(&self) -> Option<&ValOrRange> {
    self.post.pixsize()
  }

  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      preceded(
        tag_no_case(T::ELEM_NAME),
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
impl TimeElem<TimeIntervalArgs> {
  pub fn new_time_interval(start: Vec<DateTime>, stop: Vec<DateTime>) -> Result<Self, String> {
    TimeIntervalArgs::new(start, stop).map(Self::from_params)
  }

  pub fn start(&self) -> &[DateTime] {
    &self.params.start
  }
  pub fn stop(&self) -> &[DateTime] {
    &self.params.stop
  }
}
impl TimeElem<TimeStartArgs> {
  pub fn new_time_start(start: DateTime) -> Self {
    Self::from_params(TimeStartArgs { start })
  }

  pub fn start(&self) -> &DateTime {
    &self.params.start
  }
}
impl TimeElem<TimeStopArgs> {
  pub fn new_time_stop(stop: DateTime) -> Self {
    Self::from_params(TimeStopArgs { stop })
  }

  pub fn stop(&self) -> &DateTime {
    &self.params.stop
  }
}

/// Trait defining a TimeElement from its name and its parameters parsing and display methods.
pub trait TimeElemArgs: Sized + Display {
  /// Tag of the element in a STC-S string.
  const ELEM_NAME: &'static str;
  /// Parse the STC-S sub-string containing the elements.
  fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E>;
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct TimeIntervalArgs {
  #[serde(skip_serializing_if = "Vec::is_empty")]
  pub start: Vec<DateTime>,
  #[serde(skip_serializing_if = "Vec::is_empty")]
  pub stop: Vec<DateTime>,
}
impl TimeIntervalArgs {
  pub fn new(start: Vec<DateTime>, stop: Vec<DateTime>) -> Result<Self, String> {
    if start.len() == stop.len() {
      Err(format!(
        "Error in TimeIntervalArgs::new(). Start and Stop have different length: {} != {}",
        start.len(),
        stop.len()
      ))
    } else {
      Ok(Self { start, stop })
    }
  }
}
impl TimeElemArgs for TimeIntervalArgs {
  const ELEM_NAME: &'static str = "TimeInterval";
  fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      many_m_n(2, usize::MAX, preceded(multispace1, DateTime::parse)),
      |intervals| {
        #[allow(clippy::type_complexity)]
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
