use std::{
  fmt::{self, Display, Write},
  str::FromStr,
};

use serde::{Deserialize, Serialize};

use nom::{
  branch::alt,
  bytes::complete::{tag_no_case, take},
  character::complete::{char, digit0, multispace1},
  combinator::{cut, map, map_res, opt, value},
  multi::many_m_n,
  number::complete::double,
  sequence::{delimited, preceded, tuple},
  IResult,
};

use crate::{
  common::{SpaceTimeRefPos as RefPos, ValOrRange},
  NomErr,
};

/// The default value is nil.
#[derive(Serialize, Deserialize, Copy, Debug, Clone, PartialEq)]
pub enum TimeScale {
  #[serde(rename = "TT")]
  TT,
  #[serde(rename = "TDT")]
  TDT,
  #[serde(rename = "ET")]
  ET,
  #[serde(rename = "TAI")]
  TAI,
  #[serde(rename = "IAT")]
  IAT,
  #[serde(rename = "UTC")]
  UTC,
  #[serde(rename = "TEB")]
  TEB,
  #[serde(rename = "TDB")]
  TDB,
  #[serde(rename = "TCG")]
  TCG,
  #[serde(rename = "TCB")]
  TCB,
  #[serde(rename = "LST")]
  LST,
  #[serde(rename = "nil")]
  Nil,
}
impl Default for TimeScale {
  fn default() -> Self {
    Self::Nil
  }
}
impl TimeScale {
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    alt((
      value(Self::TT, tag_no_case("TT")),
      value(Self::TDB, tag_no_case("TDT")),
      value(Self::ET, tag_no_case("ET")),
      value(Self::TAI, tag_no_case("TAI")),
      value(Self::IAT, tag_no_case("IAT")),
      value(Self::UTC, tag_no_case("UTC")),
      value(Self::TEB, tag_no_case("TEB")),
      value(Self::TDB, tag_no_case("TDB")),
      value(Self::TCG, tag_no_case("TCG")),
      value(Self::TCB, tag_no_case("TCB")),
      value(Self::LST, tag_no_case("LST")),
      value(Self::Nil, tag_no_case("nil")),
    ))(input)
  }
}
impl FromStr for TimeScale {
  type Err = String;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "TT" => Ok(Self::TT),
      "TDT" => Ok(Self::TDT),
      "ET" => Ok(Self::ET),
      "TAI" => Ok(Self::TAI),
      "IAT" => Ok(Self::IAT),
      "UTC" => Ok(Self::UTC),
      "TEB" => Ok(Self::TEB),
      "TDB" => Ok(Self::TDB),
      "TCG" => Ok(Self::TCG),
      "TCB" => Ok(Self::TCB),
      "LST" => Ok(Self::LST),
      "nil" => Ok(Self::Nil),
      _ => Err(format!(
        "Unknown TimeScale. Actual: \"{}\"; Expected: one of [TT, TDT, ET, TAI, IAT, UTC, TEB, TDB, TCG, TCB, LST, nil].",
        s
      )),
    }
  }
}
impl Display for TimeScale {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str(match self {
      Self::TT => "TT",
      Self::TDT => "TDT",
      Self::ET => "ET",
      Self::TAI => "TAI",
      Self::IAT => "IAT",
      Self::UTC => "UTC",
      Self::TEB => "TEB",
      Self::TDB => "TDB",
      Self::TCG => "TCG",
      Self::TCB => "TCB",
      Self::LST => "LST",
      Self::Nil => "nil",
    })
  }
}

/// The default value is `s`;
#[derive(Serialize, Deserialize, Copy, Debug, Clone, PartialEq)]
pub enum TimeUnit {
  #[serde(rename = "s")]
  S,
  #[serde(rename = "d")]
  D,
  #[serde(rename = "a")]
  A,
  #[serde(rename = "yr")]
  Yr,
  #[serde(rename = "cy")]
  Cy,
}
impl Default for TimeUnit {
  fn default() -> Self {
    Self::S
  }
}
impl TimeUnit {
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    alt((
      value(Self::S, tag_no_case("s")),
      value(Self::D, tag_no_case("d")),
      value(Self::A, tag_no_case("a")),
      value(Self::Yr, tag_no_case("yr")),
      value(Self::Cy, tag_no_case("cy")),
    ))(input)
  }
}
impl FromStr for TimeUnit {
  type Err = String;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "s" => Ok(Self::S),
      "d" => Ok(Self::D),
      "a" => Ok(Self::A),
      "yr" => Ok(Self::Yr),
      "cy" => Ok(Self::Cy),
      _ => Err(format!(
        "Unknown Unit. Actual: \"{}\"; Expected: one of [s, d, a, yr, cy].",
        s
      )),
    }
  }
}
impl Display for TimeUnit {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str(match self {
      Self::S => "s",
      Self::D => "d",
      Self::A => "a",
      Self::Yr => "yr",
      Self::Cy => "cy",
    })
  }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub(crate) struct TimescaleRefpos {
  #[serde(skip_serializing_if = "Option::is_none")]
  pub timescale: Option<TimeScale>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub refpos: Option<RefPos>,
}
impl Display for TimescaleRefpos {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if let Some(timescale) = self.timescale.as_ref() {
      f.write_fmt(format_args!(" {}", timescale))?;
    }
    if let Some(refpos) = self.refpos.as_ref() {
      f.write_fmt(format_args!(" {}", refpos))?;
    }
    Ok(())
  }
}
impl Default for TimescaleRefpos {
  fn default() -> Self {
    Self::from_args(None, None)
  }
}
impl TimescaleRefpos {
  pub fn from_args(timescale: Option<TimeScale>, refpos: Option<RefPos>) -> Self {
    Self { timescale, refpos }
  }

  pub fn set_timescale_by_ref(&mut self, timescale: TimeScale) {
    self.timescale.replace(timescale);
  }
  pub fn set_refpos_by_ref(&mut self, refpos: RefPos) {
    self.refpos.replace(refpos);
  }

  pub fn timescale(&self) -> Option<TimeScale> {
    self.timescale
  }
  pub fn timescale_or_default(&self) -> TimeScale {
    self.timescale.unwrap_or_default()
  }
  pub fn refpos(&self) -> Option<RefPos> {
    self.refpos
  }
  pub fn refpos_or_default(&self) -> RefPos {
    self.refpos.unwrap_or_default()
  }

  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      tuple((
        opt(preceded(multispace1, TimeScale::parse::<E>)),
        opt(preceded(multispace1, RefPos::parse::<E>)),
      )),
      |(timescale, refpos)| Self { timescale, refpos },
    )(input)
  }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub(crate) struct FillfactorTimescaleRefpos {
  #[serde(skip_serializing_if = "Option::is_none")]
  /// Default value =1.0.
  pub fillfactor: Option<f64>,
  #[serde(flatten)]
  pub timescale_refpos: TimescaleRefpos,
}
impl Display for FillfactorTimescaleRefpos {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if let Some(fillfactor) = self.fillfactor {
      f.write_fmt(format_args!(" fillfactor {}", fillfactor))?;
    }
    self.timescale_refpos.fmt(f)
  }
}
impl Default for FillfactorTimescaleRefpos {
  fn default() -> Self {
    Self::from_args(None, None, None)
  }
}
impl FillfactorTimescaleRefpos {
  pub fn from_args(
    fillfactor: Option<f64>,
    timescale: Option<TimeScale>,
    refpos: Option<RefPos>,
  ) -> Self {
    Self {
      fillfactor,
      timescale_refpos: TimescaleRefpos::from_args(timescale, refpos),
    }
  }

  pub fn set_fillfactor_by_ref(&mut self, fillfactor: f64) {
    self.fillfactor.replace(fillfactor);
  }
  pub fn set_timescale_by_ref(&mut self, timescale: TimeScale) {
    self.timescale_refpos.set_timescale_by_ref(timescale);
  }
  pub fn set_refpos_by_ref(&mut self, refpos: RefPos) {
    self.timescale_refpos.set_refpos_by_ref(refpos);
  }

  pub fn fillfactor(&self) -> Option<f64> {
    self.fillfactor
  }
  pub fn fillfactor_or_default(&self) -> f64 {
    self.fillfactor().unwrap_or(1.0)
  }
  pub fn timescale(&self) -> Option<TimeScale> {
    self.timescale_refpos.timescale()
  }
  pub fn timescale_or_default(&self) -> TimeScale {
    self.timescale_refpos.timescale_or_default()
  }
  pub fn refpos(&self) -> Option<RefPos> {
    self.timescale_refpos.refpos()
  }
  pub fn refpos_or_default(&self) -> RefPos {
    self.timescale_refpos.refpos_or_default()
  }

  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      tuple((
        // fillfactor
        opt(preceded(
          delimited(multispace1, tag_no_case("fillfactor"), multispace1),
          cut(double),
        )),
        // frame
        TimescaleRefpos::parse,
      )),
      |(fillfactor, timescale_refpos)| Self {
        fillfactor,
        timescale_refpos,
      },
    )(input)
  }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum DateTime {
  #[serde(with = "iso_module")]
  Iso(Iso),
  #[serde(with = "highprecday_module")]
  JD(HighPrecDay),
  #[serde(with = "highprecday_module")]
  MJD(HighPrecDay),
}
impl Display for DateTime {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self {
      Self::Iso(e) => Display::fmt(e, f),
      Self::JD(e) => f.write_fmt(format_args!("JD {}", e)),
      Self::MJD(e) => f.write_fmt(format_args!("MJD {}", e)),
    }
  }
}
impl DateTime {
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    alt((
      map(
        preceded(
          tag_no_case("JD"),
          cut(preceded(multispace1, HighPrecDay::parse)),
        ),
        Self::JD,
      ),
      map(
        preceded(
          tag_no_case("MJD"),
          cut(preceded(multispace1, HighPrecDay::parse)),
        ),
        Self::MJD,
      ),
      map(Iso::parse, Self::Iso),
    ))(input)
  }
}

mod iso_module {
  use super::Iso;
  use nom::{
    error::{convert_error, VerboseError},
    Err,
  };
  use serde::{self, Deserialize, Deserializer, Serializer};

  pub fn serialize<S>(datetime: &Iso, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: Serializer,
  {
    serializer.serialize_str(&datetime.to_string())
  }

  pub fn deserialize<'de, D>(deserializer: D) -> Result<Iso, D::Error>
  where
    D: Deserializer<'de>,
  {
    let s = String::deserialize(deserializer)?;
    let dt = Iso::parse::<VerboseError<&str>>(s.as_str()).map_err(|err| {
      let mut msg = format!("Complete Error: {:#?}\n", err);
      msg.push_str(
        match err {
          Err::Incomplete(e) => format!("=> Incomplete Error:\n{:?}", e),
          Err::Error(e) => format!("=> Error:\n{}", convert_error(s.as_str(), e)),
          Err::Failure(e) => format!("=> Failure:\n{}", convert_error(s.as_str(), e)),
        }
        .as_str(),
      );
      serde::de::Error::custom(msg)
    })?;
    Ok(dt.1)
  }
}

mod highprecday_module {
  use super::HighPrecDay;
  use nom::{
    error::{convert_error, VerboseError},
    Err,
  };
  use serde::{self, Deserialize, Deserializer, Serializer};

  pub fn serialize<S>(day: &HighPrecDay, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: Serializer,
  {
    serializer.serialize_str(&day.to_string())
  }

  pub fn deserialize<'de, D>(deserializer: D) -> Result<HighPrecDay, D::Error>
  where
    D: Deserializer<'de>,
  {
    let s = String::deserialize(deserializer)?;
    let dt = HighPrecDay::parse::<VerboseError<&str>>(s.as_str()).map_err(|err| {
      let mut msg = format!("Complete Error: {:#?}\n", err);
      msg.push_str(
        match err {
          Err::Incomplete(e) => format!("=> Incomplete Error:\n{:?}", e),
          Err::Error(e) => format!("=> Error:\n{}", convert_error(s.as_str(), e)),
          Err::Failure(e) => format!("=> Failure:\n{}", convert_error(s.as_str(), e)),
        }
        .as_str(),
      );
      serde::de::Error::custom(msg)
    })?;
    Ok(dt.1)
  }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct YearMonthDay {
  year: u16,
  month: u8,
  day: u8,
}
impl Display for YearMonthDay {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_fmt(format_args!(
      "{:04}-{:02}-{:02}",
      self.year, self.month, self.day
    ))
  }
}
impl YearMonthDay {
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      tuple((
        map_res(take(4usize), |s: &str| {
          s.parse::<u16>().map_err(|e| e.to_string())
        }),
        delimited(
          opt(char('-')),
          map_res(take(2usize), |s: &str| {
            s.parse::<u8>().map_err(|e| e.to_string())
          }),
          opt(char('-')),
        ),
        map_res(take(2usize), |s: &str| {
          s.parse::<u8>().map_err(|e| e.to_string())
        }),
      )),
      |(year, month, day)| Self { year, month, day },
    )(input)
  }
}

/// The fractional part of a float quantity, i.e. the number after the dot.
/// WARNING:
/// * be careful with leading zeros, they are removed when parsing into integer
/// * so, to retrieve the fractional value, use: `((value as f64) / 10u64.pow(n_digits))`
#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct FractionalPart {
  /// Value of the after-the-dot parsed integer.
  /// WARNING: leading zeros are removed!!
  value: u32,
  /// Number of digits after the dot, including leading zeros
  n_digits: u8,
}
impl Display for FractionalPart {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if self.n_digits == 0 {
      assert_eq!(self.value, 0);
      f.write_char('.')
    } else {
      f.write_fmt(format_args!(
        ".{:0width$}",
        self.value,
        width = self.n_digits as usize
      ))
    }
  }
}
impl FractionalPart {
  pub(crate) fn new(value: u32, n_digits: u8) -> Self {
    Self { value, n_digits }
  }
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map_res(preceded(char('.'), digit0), |s: &str| {
      if s.is_empty() {
        Ok(FractionalPart {
          value: 0,
          n_digits: 0,
        })
      } else {
        let n_digits = s.len() as u8;
        s.parse::<u32>()
          .map_err(|e| e.to_string())
          .map(|value| Self::new(value, n_digits))
      }
    })(input)
  }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct Hms {
  hour: u8,
  minute: u8,
  second: u8,
  #[serde(skip_serializing_if = "Option::is_none")]
  second_frac: Option<FractionalPart>,
}
impl Display for Hms {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if let Some(frac) = &self.second_frac {
      f.write_fmt(format_args!(
        "{:02}:{:02}:{:02}{}",
        self.hour, self.minute, self.second, frac
      ))
    } else {
      f.write_fmt(format_args!(
        "{:02}:{:02}:{:02}",
        self.hour, self.minute, self.second
      ))
    }
  }
}
impl Hms {
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      tuple((
        map_res(take(2usize), |s: &str| {
          s.parse::<u8>().map_err(|e| e.to_string())
        }),
        delimited(
          opt(char(':')),
          map_res(take(2usize), |s: &str| {
            s.parse::<u8>().map_err(|e| e.to_string())
          }),
          opt(char(':')),
        ),
        // SS
        map_res(take(2usize), |s: &str| {
          s.parse::<u8>().map_err(|e| e.to_string())
        }),
        // .SSS
        opt(FractionalPart::parse),
      )),
      |(hour, minute, second, second_frac)| Self {
        hour,
        minute,
        second,
        second_frac,
      },
    )(input)
  }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct Iso {
  ymd: YearMonthDay,
  #[serde(skip_serializing_if = "Option::is_none")]
  hms: Option<Hms>,
}
impl Display for Iso {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if let Some(hms) = &self.hms {
      f.write_fmt(format_args!("{}T{}Z", &self.ymd, hms))
    } else {
      Display::fmt(&self.ymd, f)
    }
  }
}
impl Iso {
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      tuple((
        YearMonthDay::parse,
        opt(delimited(char('T'), Hms::parse, opt(char('Z')))),
      )),
      |(ymd, hms)| Self { ymd, hms },
    )(input)
  }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct HighPrecDay {
  #[serde(skip_serializing_if = "Option::is_none")]
  minus_sign: Option<bool>,
  day: u32,
  #[serde(skip_serializing_if = "Option::is_none")]
  day_frac: Option<FractionalPart>,
}
impl Display for HighPrecDay {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if let Some(minus_sign) = self.minus_sign {
      let sign = if minus_sign { '-' } else { '+' };
      f.write_char(sign)?;
    }
    if let Some(day_frac) = &self.day_frac {
      f.write_fmt(format_args!("{}{}", self.day, day_frac))
    } else {
      f.write_fmt(format_args!("{}", self.day))
    }
  }
}
impl HighPrecDay {
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      tuple((
        opt(alt((map(char('-'), |_| true), map(char('+'), |_| false)))),
        map_res(digit0, |s: &str| {
          if s.is_empty() {
            Ok(0)
          } else {
            s.parse::<u32>().map_err(|e| e.to_string())
          }
        }),
        opt(FractionalPart::parse),
      )),
      |(minus_sign, day, day_frac)| Self {
        minus_sign,
        day,
        day_frac,
      },
    )(input)
  }
}

#[derive(Serialize, Deserialize, Default, Debug, PartialEq)]
pub(crate) struct FromUnitToPixSize {
  #[serde(skip_serializing_if = "Option::is_none")]
  pub unit: Option<TimeUnit>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub error: Option<ValOrRange>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub resolution: Option<ValOrRange>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub size: Option<ValOrRange>,
  #[serde(rename = "pixsize", skip_serializing_if = "Option::is_none")]
  pub pixsize: Option<ValOrRange>,
}
impl FromUnitToPixSize {
  // Setters by ref

  pub fn set_unit_by_ref(&mut self, unit: TimeUnit) {
    self.unit.replace(unit);
  }
  pub fn set_error_by_ref(&mut self, error: ValOrRange) {
    self.error.replace(error);
  }
  pub fn set_resolution_by_ref(&mut self, resolution: ValOrRange) {
    self.resolution.replace(resolution);
  }
  pub fn set_size_by_ref(&mut self, size: ValOrRange) {
    self.size.replace(size);
  }
  pub fn set_pixsize_by_ref(&mut self, pixsize: ValOrRange) {
    self.pixsize.replace(pixsize);
  }

  // Getters

  pub fn unit(&self) -> Option<TimeUnit> {
    self.unit
  }
  pub fn unit_or_default(&self) -> TimeUnit {
    self.unit.unwrap_or_default()
  }
  pub fn error(&self) -> Option<&ValOrRange> {
    self.error.as_ref()
  }
  pub fn resolution(&self) -> Option<&ValOrRange> {
    self.resolution.as_ref()
  }
  pub fn size(&self) -> Option<&ValOrRange> {
    self.size.as_ref()
  }
  pub fn pixsize(&self) -> Option<&ValOrRange> {
    self.pixsize.as_ref()
  }

  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      tuple((
        // unit
        opt(preceded(
          preceded(multispace1, tag_no_case("unit")),
          TimeUnit::parse::<E>,
        )),
        // error
        opt(preceded(
          preceded(multispace1, tag_no_case("Error")),
          cut(many_m_n(1, 2, preceded(multispace1, double))),
        )),
        // resolution
        opt(preceded(
          preceded(multispace1, tag_no_case("Resolution")),
          cut(many_m_n(1, 2, preceded(multispace1, double))),
        )),
        // size
        opt(preceded(
          preceded(multispace1, tag_no_case("Size")),
          cut(many_m_n(1, 2, preceded(multispace1, double))),
        )),
        // pixsize
        opt(preceded(
          preceded(multispace1, tag_no_case("PixSize")),
          cut(many_m_n(1, 2, preceded(multispace1, double))),
        )),
      )),
      |(unit, error, resolution, size, pixsize)| Self {
        unit,
        error: error.map(|v| v.into()),
        resolution: resolution.map(|v| v.into()),
        size: size.map(|v| v.into()),
        pixsize: pixsize.map(|v| v.into()),
      },
    )(input)
  }
}
impl Display for FromUnitToPixSize {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if let Some(unit) = self.unit.as_ref() {
      f.write_fmt(format_args!(" unit {}", unit))?;
    }
    if let Some(error) = self.error.as_ref() {
      f.write_fmt(format_args!(" Error {}", error))?;
    }
    if let Some(resolution) = self.resolution.as_ref() {
      f.write_fmt(format_args!(" Resolution {}", resolution))?;
    }
    if let Some(size) = self.size.as_ref() {
      f.write_fmt(format_args!(" Size {}", size))?;
    }
    if let Some(pixsize) = self.pixsize.as_ref() {
      f.write_fmt(format_args!(" PixSize {}", pixsize))?;
    }
    Ok(())
  }
}

#[derive(Serialize, Deserialize, Default, Debug, PartialEq)]
pub(crate) struct FromTimeToPixSize {
  #[serde(skip_serializing_if = "Option::is_none")]
  time: Option<DateTime>,
  #[serde(flatten)]
  from_unit_to_pixsize: FromUnitToPixSize,
}
impl FromTimeToPixSize {
  pub fn set_time_by_ref(&mut self, time: DateTime) {
    self.time.replace(time);
  }
  pub fn set_unit_by_ref(&mut self, unit: TimeUnit) {
    self.from_unit_to_pixsize.set_unit_by_ref(unit)
  }
  pub fn set_error_by_ref(&mut self, error: ValOrRange) {
    self.from_unit_to_pixsize.set_error_by_ref(error)
  }
  pub fn set_resolution_by_ref(&mut self, resolution: ValOrRange) {
    self.from_unit_to_pixsize.set_resolution_by_ref(resolution)
  }
  pub fn set_size_by_ref(&mut self, size: ValOrRange) {
    self.from_unit_to_pixsize.set_size_by_ref(size)
  }
  pub fn set_pixsize_by_ref(&mut self, pixsize: ValOrRange) {
    self.from_unit_to_pixsize.set_pixsize_by_ref(pixsize)
  }

  pub fn time(&self) -> Option<&DateTime> {
    self.time.as_ref()
  }
  pub fn unit(&self) -> Option<TimeUnit> {
    self.from_unit_to_pixsize.unit()
  }
  pub fn unit_or_default(&self) -> TimeUnit {
    self.from_unit_to_pixsize.unit_or_default()
  }
  pub fn error(&self) -> Option<&ValOrRange> {
    self.from_unit_to_pixsize.error()
  }
  pub fn resolution(&self) -> Option<&ValOrRange> {
    self.from_unit_to_pixsize.resolution()
  }
  pub fn size(&self) -> Option<&ValOrRange> {
    self.from_unit_to_pixsize.size()
  }
  pub fn pixsize(&self) -> Option<&ValOrRange> {
    self.from_unit_to_pixsize.pixsize()
  }

  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      tuple((
        opt(preceded(
          preceded(multispace1, tag_no_case("Time")),
          cut(preceded(multispace1, DateTime::parse)),
        )),
        FromUnitToPixSize::parse,
      )),
      |(time, from_unit_to_pixsize)| Self {
        time,
        from_unit_to_pixsize,
      },
    )(input)
  }
}
impl Display for FromTimeToPixSize {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if let Some(time) = self.time.as_ref() {
      f.write_fmt(format_args!(" Time {}", time))?;
    }
    Display::fmt(&self.from_unit_to_pixsize, f)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use nom::error::VerboseError;

  #[test]
  fn test_timescale_round_trip() {
    assert_eq!(
      TimeScale::from_str(TimeScale::TT.to_string().as_ref()),
      Ok(TimeScale::TT)
    );
    assert_eq!(
      TimeScale::from_str(TimeScale::TDT.to_string().as_ref()),
      Ok(TimeScale::TDT)
    );
    assert_eq!(
      TimeScale::from_str(TimeScale::ET.to_string().as_ref()),
      Ok(TimeScale::ET)
    );
    assert_eq!(
      TimeScale::from_str(TimeScale::TAI.to_string().as_ref()),
      Ok(TimeScale::TAI)
    );
    assert_eq!(
      TimeScale::from_str(TimeScale::IAT.to_string().as_ref()),
      Ok(TimeScale::IAT)
    );
    assert_eq!(
      TimeScale::from_str(TimeScale::UTC.to_string().as_ref()),
      Ok(TimeScale::UTC)
    );
    assert_eq!(
      TimeScale::from_str(TimeScale::TEB.to_string().as_ref()),
      Ok(TimeScale::TEB)
    );
    assert_eq!(
      TimeScale::from_str(TimeScale::TDB.to_string().as_ref()),
      Ok(TimeScale::TDB)
    );
    assert_eq!(
      TimeScale::from_str(TimeScale::TCG.to_string().as_ref()),
      Ok(TimeScale::TCG)
    );
    assert_eq!(
      TimeScale::from_str(TimeScale::TCB.to_string().as_ref()),
      Ok(TimeScale::TCB)
    );
    assert_eq!(
      TimeScale::from_str(TimeScale::LST.to_string().as_ref()),
      Ok(TimeScale::LST)
    );
    assert_eq!(
      TimeScale::from_str(TimeScale::Nil.to_string().as_ref()),
      Ok(TimeScale::Nil)
    );
  }

  #[test]
  fn test_timeunit_round_trip() {
    assert_eq!(
      TimeUnit::from_str(TimeUnit::S.to_string().as_ref()),
      Ok(TimeUnit::S)
    );
    assert_eq!(
      TimeUnit::from_str(TimeUnit::D.to_string().as_ref()),
      Ok(TimeUnit::D)
    );
    assert_eq!(
      TimeUnit::from_str(TimeUnit::A.to_string().as_ref()),
      Ok(TimeUnit::A)
    );
    assert_eq!(
      TimeUnit::from_str(TimeUnit::Yr.to_string().as_ref()),
      Ok(TimeUnit::Yr)
    );
    assert_eq!(
      TimeUnit::from_str(TimeUnit::Cy.to_string().as_ref()),
      Ok(TimeUnit::Cy)
    );
  }

  #[test]
  fn test_ymd_1() {
    let ymd = YearMonthDay {
      year: 2023,
      month: 11,
      day: 23,
    };
    let ymd1 = YearMonthDay::parse::<VerboseError<&str>>("20231123")
      .unwrap()
      .1;
    let ymd2 = YearMonthDay::parse::<VerboseError<&str>>("2023-11-23")
      .unwrap()
      .1;
    assert_eq!(ymd, ymd1);
    assert_eq!(ymd, ymd2);
    assert_eq!(ymd.to_string(), "2023-11-23");
    assert_eq!(ymd1.to_string(), "2023-11-23");
    assert_eq!(ymd2.to_string(), "2023-11-23");
  }
  #[test]
  fn test_ymd_2() {
    let ymd = YearMonthDay {
      year: 2000,
      month: 1,
      day: 2,
    };
    let ymd1 = YearMonthDay::parse::<VerboseError<&str>>("20000102")
      .unwrap()
      .1;
    let ymd2 = YearMonthDay::parse::<VerboseError<&str>>("2000-01-02")
      .unwrap()
      .1;
    assert_eq!(ymd, ymd1);
    assert_eq!(ymd, ymd2);
    assert_eq!(ymd.to_string(), "2000-01-02");
    assert_eq!(ymd1.to_string(), "2000-01-02");
    assert_eq!(ymd2.to_string(), "2000-01-02");
  }

  #[test]
  fn test_hms_1() {
    let hms = Hms {
      hour: 0,
      minute: 0,
      second: 0,
      second_frac: None,
    };
    let hms1 = Hms::parse::<VerboseError<&str>>("000000").unwrap().1;
    let hms2 = Hms::parse::<VerboseError<&str>>("00:00:00").unwrap().1;
    assert_eq!(hms, hms1);
    assert_eq!(hms, hms2);
    assert_eq!(hms.to_string(), "00:00:00");
    assert_eq!(hms1.to_string(), "00:00:00");
    assert_eq!(hms2.to_string(), "00:00:00");
  }

  #[test]
  fn test_hms_2() {
    let hms = Hms {
      hour: 0,
      minute: 0,
      second: 0,
      second_frac: Some(FractionalPart::new(0, 0)),
    };
    let hms1 = Hms::parse::<VerboseError<&str>>("000000.").unwrap().1;
    let hms2 = Hms::parse::<VerboseError<&str>>("00:00:00.").unwrap().1;
    assert_eq!(hms, hms1);
    assert_eq!(hms, hms2);
    assert_eq!(hms.to_string(), "00:00:00.");
    assert_eq!(hms1.to_string(), "00:00:00.");
    assert_eq!(hms2.to_string(), "00:00:00.");
  }

  #[test]
  fn test_hms_3() {
    let hms = Hms {
      hour: 0,
      minute: 0,
      second: 0,
      second_frac: Some(FractionalPart::new(0, 10)),
    };
    let hms1 = Hms::parse::<VerboseError<&str>>("000000.0000000000")
      .unwrap()
      .1;
    let hms2 = Hms::parse::<VerboseError<&str>>("00:00:00.0000000000")
      .unwrap()
      .1;
    assert_eq!(hms, hms1);
    assert_eq!(hms, hms2);
    assert_eq!(hms.to_string(), "00:00:00.0000000000");
    assert_eq!(hms1.to_string(), "00:00:00.0000000000");
    assert_eq!(hms2.to_string(), "00:00:00.0000000000");
  }

  #[test]
  fn test_hms_4() {
    let hms = Hms {
      hour: 24,
      minute: 01,
      second: 59,
      second_frac: Some(FractionalPart::new(123, 5)),
    };
    let hms1 = Hms::parse::<VerboseError<&str>>("240159.00123").unwrap().1;
    let hms2 = Hms::parse::<VerboseError<&str>>("24:01:59.00123")
      .unwrap()
      .1;
    assert_eq!(hms, hms1);
    assert_eq!(hms, hms2);
    assert_eq!(hms.to_string(), "24:01:59.00123");
    assert_eq!(hms1.to_string(), "24:01:59.00123");
    assert_eq!(hms2.to_string(), "24:01:59.00123");
  }

  #[test]
  fn test_highprecday() {
    let day = HighPrecDay {
      minus_sign: None,
      day: 0,
      day_frac: None,
    };
    let day_str = day.to_string();
    assert_eq!(&day_str, "0");
    assert_eq!(
      HighPrecDay::parse::<VerboseError<&str>>(&day_str)
        .unwrap()
        .1,
      day
    );

    let day = HighPrecDay {
      minus_sign: Some(false),
      day: 0,
      day_frac: None,
    };
    let day_str = day.to_string();
    assert_eq!(&day_str, "+0");
    assert_eq!(
      HighPrecDay::parse::<VerboseError<&str>>(&day_str)
        .unwrap()
        .1,
      day
    );

    let day = HighPrecDay {
      minus_sign: Some(true),
      day: 0,
      day_frac: None,
    };
    let day_str = day.to_string();
    assert_eq!(&day_str, "-0");
    assert_eq!(
      HighPrecDay::parse::<VerboseError<&str>>(&day_str)
        .unwrap()
        .1,
      day
    );
  }
}
