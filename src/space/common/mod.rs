use std::{
  fmt::{self, Display},
  str::FromStr,
};

use serde::{Deserialize, Serialize};

use nom::{
  branch::alt,
  bytes::complete::tag,
  character::complete::multispace1,
  combinator::{cut, map, opt, value},
  multi::many1,
  number::complete::double,
  sequence::{delimited, preceded, tuple},
  IResult,
};

pub mod compound;
pub mod expression;
pub mod region;
pub mod velocity;

use crate::{common::SpaceTimeRefPos as RefPos, NomErr};
use velocity::Velocity;

/// `Ecliptic` assume to have epoch `J2000` with respect to `ICRS`.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum Frame {
  ICRS,
  FK5,
  FK4,
  J2000,
  B1950,
  #[serde(rename = "ECLIPTIC")]
  Ecliptic,
  #[serde(rename = "GALACTIC")]
  Galactic,
  #[serde(rename = "GALACTIC_II")]
  Galactic2,
  #[serde(rename = "SUPER_GALACTIC")]
  SuperGalactic,
  #[serde(rename = "GEO_C")]
  GeoC,
  #[serde(rename = "GEO_D")]
  GeoD,
  #[serde(rename = "UNKNOWNFrame")]
  UnknownFrame,
}
impl Frame {
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    alt((
      value(Self::ICRS, tag("ICRS")),
      value(Self::FK5, tag("FK5")),
      value(Self::FK4, tag("FK4")),
      value(Self::J2000, tag("J2000")),
      value(Self::B1950, tag("B1950")),
      value(Self::Ecliptic, tag("ECLIPTIC")),
      value(Self::Galactic2, tag("GALACTIC_II")),
      value(Self::Galactic, tag("GALACTIC")),
      value(Self::SuperGalactic, tag("SUPER_GALACTIC")),
      value(Self::GeoC, tag("GEO_C")),
      value(Self::GeoD, tag("GEO_D")),
      value(Self::UnknownFrame, tag("UNKNOWNFrame")),
    ))(input)
  }
}
impl FromStr for Frame {
  type Err = String;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "ICRS" => Ok(Self::ICRS),
      "FK5" => Ok(Self::FK5),
      "FK4" => Ok(Self::FK4),
      "J2000" => Ok(Self::J2000),
      "B1950" => Ok(Self::B1950),
      "ECLIPTIC" => Ok(Self::Ecliptic),
      "GALACTIC" => Ok(Self::Galactic),
      "GALACTIC_II" => Ok(Self::Galactic2),
      "SUPER_GALACTIC" => Ok(Self::SuperGalactic),
      "GEO_C" => Ok(Self::GeoC),
      "GEO_D" => Ok(Self::GeoD),
      "UNKNOWNFrame" => Ok(Self::UnknownFrame),
      _ => Err(format!("Unknown frame. Actual: \"{}\"; Expected: one of [ICRS, FK5, FK4, J2000, B1950, ECLIPTIC, GALACTIC, GALACTIC_II, SUPER_GALACTIC, GEO_C, GEO_D, UNKNOWNFrame].", s))
    }
  }
}
impl Display for Frame {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str(match self {
      Self::ICRS => "ICRS",
      Self::FK5 => "FK5",
      Self::FK4 => "FK4",
      Self::J2000 => "J2000",
      Self::B1950 => "B1950",
      Self::Ecliptic => "ECLIPTIC",
      Self::Galactic => "GALACTIC",
      Self::Galactic2 => "GALACTIC_II",
      Self::SuperGalactic => "SUPER_GALACTIC",
      Self::GeoC => "GEO_C",
      Self::GeoD => "GEO_D",
      Self::UnknownFrame => "UNKNOWNFrame",
    })
  }
}

/// The default value is `SPHER2`, except for `Convex`, where the default is `UNITSPHER`.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum Flavor {
  #[serde(rename = "SPHER2")]
  Spher2,
  #[serde(rename = "UNITSPHER")]
  UnitSpher,
  #[serde(rename = "CART1")]
  Cart1,
  #[serde(rename = "CART2")]
  Cart2,
  #[serde(rename = "CART3")]
  Cart3,
  #[serde(rename = "SPHER3")]
  Spher3,
}
impl Flavor {
  /// Returns the number of spacial coordinates of the frama.
  pub fn dim(&self) -> u8 {
    match self {
      Self::Spher2 => 2,
      Self::UnitSpher => 3,
      Self::Cart1 => 1,
      Self::Cart2 => 2,
      Self::Cart3 => 3,
      Self::Spher3 => 3,
    }
  }
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    alt((
      value(Self::Spher2, alt((tag("SPHER2"), tag("SPHERICAL2")))),
      value(Self::UnitSpher, tag("UNITSPHER")),
      value(Self::Cart1, alt((tag("CART1"), tag("CARTESIAN1")))),
      value(Self::Cart2, alt((tag("CART2"), tag("CARTESIAN2")))),
      value(Self::Cart3, alt((tag("CART3"), tag("CARTESIAN3")))),
      value(Self::Spher3, alt((tag("SPHER3"), tag("SPHERICAL3")))),
    ))(input)
  }
}
impl FromStr for Flavor {
  type Err = String;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    // Support aliases to be compatible with TAP version of STC-S (see section 6 of TAP)
    match s {
      "SPHER2" | "SPHERICAL2" => Ok(Self::Spher2),
      "UNITSPHER" => Ok(Self::UnitSpher),
      "CART1" | "CARTESIAN1" => Ok(Self::Cart1),
      "CART2" | "CARTESIAN2" => Ok(Self::Cart2),
      "CART3" | "CARTESIAN3" => Ok(Self::Cart3),
      "SPHER3" | "SPHERICAL3" => Ok(Self::Spher3),
      _ => Err(format!(
        "Unknown Flavor. Actual: \"{}\"; Expected: one of [SPHER2, UNITSPHER, CART1, CART2, CART3, SPHER3].",
        s
      )),
    }
  }
}
impl Display for Flavor {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str(match self {
      Self::Spher2 => "SPHER2",
      Self::UnitSpher => "UNITSPHER",
      Self::Cart1 => "CART1",
      Self::Cart2 => "CART2",
      Self::Cart3 => "CART3",
      Self::Spher3 => "SPHER3",
    })
  }
}

/// The default value for spherical coordinates, except `GEO`, is `deg`;
/// for `GEO`: `deg deg m`; for Cartesian coordinates: `m`.
/// For multi-dimensional frames one may specify a unit for each axis, such as `deg deg m`.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum SpaceUnit {
  #[serde(rename = "deg")]
  Deg,
  #[serde(rename = "arcmin")]
  Arcmin,
  #[serde(rename = "arcsec")]
  Arcsec,
  #[serde(rename = "m")]
  M,
  #[serde(rename = "mm")]
  Mm,
  #[serde(rename = "km")]
  Km,
  AU,
  #[serde(rename = "pc")]
  Pc,
  #[serde(rename = "kpc")]
  Kpc,
  Mpc,
}
impl SpaceUnit {
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    alt((
      value(Self::Deg, tag("deg")),
      value(Self::Arcmin, tag("arcmin")),
      value(Self::Arcsec, tag("arcsec")),
      value(Self::Mm, tag("mm")),
      value(Self::M, tag("m")),
      value(Self::Km, tag("km")),
      value(Self::AU, tag("AU")),
      value(Self::Pc, tag("pc")),
      value(Self::Kpc, tag("kpc")),
      value(Self::Mpc, tag("Mpc")),
    ))(input)
  }
}
impl FromStr for SpaceUnit {
  type Err = String;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "deg" => Ok(Self::Deg),
      "arcmin" => Ok(Self::Arcmin),
      "arcsec" => Ok(Self::Arcsec),
      "m" => Ok(Self::M),
      "mm" => Ok(Self::Mm),
      "km" => Ok(Self::Km),
      "AU" => Ok(Self::AU),
      "pc" => Ok(Self::Pc),
      "kpc" => Ok(Self::Kpc),
      "Mpc" => Ok(Self::Mpc),
      _ => Err(format!(
        "Unknown Unit. Actual: \"{}\"; Expected: one of [deg, arcmin, arcsec, m, mm, km, AU, pc, kpc, Mpc].",
        s
      )),
    }
  }
}
impl Display for SpaceUnit {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str(match self {
      Self::Deg => "deg",
      Self::Arcmin => "arcmin",
      Self::Arcsec => "arcsec",
      Self::M => "m",
      Self::Mm => "mm",
      Self::Km => "km",
      Self::AU => "AU",
      Self::Pc => "pc",
      Self::Kpc => "kpc",
      Self::Mpc => "Mpc",
    })
  }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct FrameRefposFlavor {
  pub frame: Frame,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub refpos: Option<RefPos>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub flavor: Option<Flavor>,
}
impl Display for FrameRefposFlavor {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_fmt(format_args!(" {}", self.frame))?;
    if let Some(refpos) = self.refpos.as_ref() {
      f.write_fmt(format_args!(" {}", refpos))?;
    }
    if let Some(flavor) = self.flavor.as_ref() {
      f.write_fmt(format_args!(" {}", flavor))?;
    }
    Ok(())
  }
}
impl FrameRefposFlavor {
  pub fn new(frame: Frame, refpos: Option<RefPos>, flavor: Option<Flavor>) -> Self {
    Self {
      frame,
      refpos,
      flavor,
    }
  }
  pub fn from_frame(frame: Frame) -> Self {
    Self::new(frame, None, None)
  }
  pub fn set_refpos(mut self, refpos: RefPos) -> Self {
    self.set_refpos_by_ref(refpos);
    self
  }
  pub fn set_flavor(mut self, flavor: Flavor) -> Self {
    self.set_flavor_by_ref(flavor);
    self
  }
  pub fn set_refpos_by_ref(&mut self, refpos: RefPos) {
    self.refpos = Some(refpos);
  }
  pub fn set_flavor_by_ref(&mut self, flavor: Flavor) {
    self.flavor = Some(flavor);
  }
  pub fn frame(&self) -> &Frame {
    &self.frame
  }
  pub fn refpos(&self) -> Option<&RefPos> {
    self.refpos.as_ref()
  }
  pub fn flavor(&self) -> Option<&Flavor> {
    self.flavor.as_ref()
  }
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      tuple((
        // frame: Support 'optional' instead of 'mandatory' to be compatible with TAP version of STC-S, see section 6 of TAP
        opt(preceded(multispace1, Frame::parse::<E>)),
        // refpos
        opt(preceded(multispace1, RefPos::parse::<E>)),
        // flavor
        opt(preceded(multispace1, Flavor::parse::<E>)),
      )),
      |(frame, refpos, flavor)| Self {
        frame: frame.unwrap_or(Frame::UnknownFrame),
        refpos,
        flavor,
      },
    )(input)
  }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct FillFrameRefposFlavor {
  #[serde(skip_serializing_if = "Option::is_none")]
  /// Default value =1.0.
  pub fillfactor: Option<f64>,
  #[serde(flatten)]
  pub frame_refpos_flavor: FrameRefposFlavor,
}
impl Display for FillFrameRefposFlavor {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if let Some(fillfactor) = self.fillfactor {
      f.write_fmt(format_args!(" fillfactor {}", fillfactor))?;
    }
    self.frame_refpos_flavor.fmt(f)
  }
}
impl FillFrameRefposFlavor {
  pub fn new(
    fillfactor: Option<f64>,
    frame: Frame,
    refpos: Option<RefPos>,
    flavor: Option<Flavor>,
  ) -> Self {
    Self {
      fillfactor,
      frame_refpos_flavor: FrameRefposFlavor::new(frame, refpos, flavor),
    }
  }
  pub fn from_frame(frame: Frame) -> Self {
    Self::new(None, frame, None, None)
  }
  pub fn set_fillfactor(mut self, fillfactor: f64) -> Self {
    self.set_fillfactor_by_ref(fillfactor);
    self
  }
  pub fn set_refpos(mut self, refpos: RefPos) -> Self {
    self.set_refpos_by_ref(refpos);
    self
  }
  pub fn set_flavor(mut self, flavor: Flavor) -> Self {
    self.set_flavor_by_ref(flavor);
    self
  }
  pub fn set_fillfactor_by_ref(&mut self, fillfactor: f64) {
    self.fillfactor = Some(fillfactor);
  }
  pub fn set_refpos_by_ref(&mut self, refpos: RefPos) {
    self.frame_refpos_flavor.set_refpos_by_ref(refpos);
  }
  pub fn set_flavor_by_ref(&mut self, flavor: Flavor) {
    self.frame_refpos_flavor.set_flavor_by_ref(flavor);
  }
  pub fn fillfactor(&self) -> Option<f64> {
    self.fillfactor
  }
  pub fn frame(&self) -> &Frame {
    self.frame_refpos_flavor.frame()
  }
  pub fn refpos(&self) -> Option<&RefPos> {
    self.frame_refpos_flavor.refpos()
  }
  pub fn flavor(&self) -> Option<&Flavor> {
    self.frame_refpos_flavor.flavor()
  }
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      tuple((
        // fillfactor
        opt(preceded(
          delimited(multispace1, tag("fillfactor"), multispace1),
          cut(double),
        )),
        // frame
        FrameRefposFlavor::parse,
      )),
      |(fillfactor, frame_refpos_flavor)| Self {
        fillfactor,
        frame_refpos_flavor,
      },
    )(input)
  }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct FromPosToVelocity {
  #[serde(skip_serializing_if = "Option::is_none")]
  pub position: Option<Vec<f64>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub unit: Option<Vec<SpaceUnit>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  /// Multiple errors possible.
  pub error: Option<Vec<f64>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  /// Multiple resolutions possible.
  pub resolution: Option<Vec<f64>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  /// Multiple sizes possible
  pub size: Option<Vec<f64>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  /// Multiple pixsizes possible
  pub pixsize: Option<Vec<f64>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub velocity: Option<Velocity>,
}
impl FromPosToVelocity {
  pub fn parse<'a, E: NomErr<'a>>(input: &'a str) -> IResult<&'a str, Self, E> {
    map(
      tuple((
        // position
        opt(preceded(
          preceded(multispace1, tag("Position")),
          cut(many1(preceded(multispace1, double))),
        )),
        // unit
        opt(preceded(
          preceded(multispace1, tag("unit")),
          cut(many1(preceded(multispace1, SpaceUnit::parse::<E>))),
        )),
        // error
        opt(preceded(
          preceded(multispace1, tag("Error")),
          cut(many1(preceded(multispace1, double))),
        )),
        // resolution
        opt(preceded(
          preceded(multispace1, tag("Resolution")),
          cut(many1(preceded(multispace1, double))),
        )),
        // size
        opt(preceded(
          preceded(multispace1, tag("Size")),
          cut(many1(preceded(multispace1, double))),
        )),
        // pixsize
        opt(preceded(
          preceded(multispace1, tag("PixSize")),
          cut(many1(preceded(multispace1, double))),
        )),
        // velocity
        opt(preceded(multispace1, Velocity::parse)),
      )),
      |(position, unit, error, resolution, size, pixsize, velocity)| Self {
        position,
        unit,
        error,
        resolution,
        size,
        pixsize,
        velocity,
      },
    )(input)
  }
}
impl Display for FromPosToVelocity {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if let Some(position) = self.position.as_ref() {
      f.write_str(" Position")?;
      for pos in position {
        f.write_fmt(format_args!(" {}", pos))?;
      }
    }
    if let Some(unit) = self.unit.as_ref() {
      f.write_str(" unit")?;
      for u in unit {
        f.write_fmt(format_args!(" {}", u))?;
      }
    }
    if let Some(errors) = self.error.as_ref() {
      f.write_str(" Error")?;
      for e in errors {
        f.write_fmt(format_args!(" {}", e))?;
      }
    }
    if let Some(resolutions) = self.resolution.as_ref() {
      f.write_str(" Resolution")?;
      for r in resolutions {
        f.write_fmt(format_args!(" {}", r))?;
      }
    }
    if let Some(sizes) = self.size.as_ref() {
      f.write_str(" Size")?;
      for p in sizes {
        f.write_fmt(format_args!(" {}", p))?;
      }
    }
    if let Some(pixsizes) = self.pixsize.as_ref() {
      f.write_str(" PixSize")?;
      for p in pixsizes {
        f.write_fmt(format_args!(" {}", p))?;
      }
    }
    if let Some(velocity) = self.velocity.as_ref() {
      velocity.fmt(f)
    } else {
      Ok(())
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_frame_from_string() {
    assert_eq!(Ok(Frame::ICRS), Frame::from_str("ICRS"));
    assert_eq!(Ok(Frame::FK5), Frame::from_str("FK5"));
    assert_eq!(Ok(Frame::FK4), Frame::from_str("FK4"));
    assert_eq!(Ok(Frame::J2000), Frame::from_str("J2000"));
    assert_eq!(Ok(Frame::B1950), Frame::from_str("B1950"));
    assert_eq!(Ok(Frame::Ecliptic), Frame::from_str("ECLIPTIC"));
    assert_eq!(Ok(Frame::Galactic), Frame::from_str("GALACTIC"));
    assert_eq!(Ok(Frame::Galactic2), Frame::from_str("GALACTIC_II"));
    assert_eq!(Ok(Frame::SuperGalactic), Frame::from_str("SUPER_GALACTIC"));
    assert_eq!(Ok(Frame::GeoC), Frame::from_str("GEO_C"));
    assert_eq!(Ok(Frame::GeoD), Frame::from_str("GEO_D"));
    assert_eq!(Ok(Frame::UnknownFrame), Frame::from_str("UNKNOWNFrame"));
  }

  #[test]
  fn test_frame_to_string() {
    assert_eq!(Frame::ICRS.to_string(), "ICRS");
    assert_eq!(Frame::FK5.to_string(), "FK5");
    assert_eq!(Frame::FK4.to_string(), "FK4");
    assert_eq!(Frame::J2000.to_string(), "J2000");
    assert_eq!(Frame::B1950.to_string(), "B1950");
    assert_eq!(Frame::Ecliptic.to_string(), "ECLIPTIC");
    assert_eq!(Frame::Galactic.to_string(), "GALACTIC");
    assert_eq!(Frame::Galactic2.to_string(), "GALACTIC_II");
    assert_eq!(Frame::SuperGalactic.to_string(), "SUPER_GALACTIC");
    assert_eq!(Frame::GeoC.to_string(), "GEO_C");
    assert_eq!(Frame::GeoD.to_string(), "GEO_D");
    assert_eq!(Frame::UnknownFrame.to_string(), "UNKNOWNFrame");
  }

  #[test]
  fn test_frame_round_trip() {
    assert_eq!(
      Frame::from_str(Frame::ICRS.to_string().as_str()),
      Ok(Frame::ICRS)
    );
    assert_eq!(
      Frame::from_str(Frame::FK5.to_string().as_str()),
      Ok(Frame::FK5)
    );
    assert_eq!(
      Frame::from_str(Frame::FK4.to_string().as_str()),
      Ok(Frame::FK4)
    );
    assert_eq!(
      Frame::from_str(Frame::J2000.to_string().as_str()),
      Ok(Frame::J2000)
    );
    assert_eq!(
      Frame::from_str(Frame::B1950.to_string().as_str()),
      Ok(Frame::B1950)
    );
    assert_eq!(
      Frame::from_str(Frame::Ecliptic.to_string().as_str()),
      Ok(Frame::Ecliptic)
    );
    assert_eq!(
      Frame::from_str(Frame::Galactic.to_string().as_str()),
      Ok(Frame::Galactic)
    );
    assert_eq!(
      Frame::from_str(Frame::Galactic2.to_string().as_str()),
      Ok(Frame::Galactic2)
    );
    assert_eq!(
      Frame::from_str(Frame::SuperGalactic.to_string().as_str()),
      Ok(Frame::SuperGalactic)
    );
    assert_eq!(
      Frame::from_str(Frame::GeoC.to_string().as_str()),
      Ok(Frame::GeoC)
    );
    assert_eq!(
      Frame::from_str(Frame::GeoD.to_string().as_str()),
      Ok(Frame::GeoD)
    );
    assert_eq!(
      Frame::from_str(Frame::UnknownFrame.to_string().as_str()),
      Ok(Frame::UnknownFrame)
    );
  }

  #[test]
  fn tets_refpos_round_trip() {
    assert_eq!(
      RefPos::from_str(RefPos::Geocenter.to_string().as_ref()),
      Ok(RefPos::Geocenter)
    );
    assert_eq!(
      RefPos::from_str(RefPos::Barycenter.to_string().as_ref()),
      Ok(RefPos::Barycenter)
    );
    assert_eq!(
      RefPos::from_str(RefPos::Heliocenter.to_string().as_ref()),
      Ok(RefPos::Heliocenter)
    );
    assert_eq!(
      RefPos::from_str(RefPos::Topocenter.to_string().as_ref()),
      Ok(RefPos::Topocenter)
    );
    assert_eq!(
      RefPos::from_str(RefPos::GalacticCenter.to_string().as_ref()),
      Ok(RefPos::GalacticCenter)
    );
    assert_eq!(
      RefPos::from_str(RefPos::Embarycenter.to_string().as_ref()),
      Ok(RefPos::Embarycenter)
    );
    assert_eq!(
      RefPos::from_str(RefPos::Moon.to_string().as_ref()),
      Ok(RefPos::Moon)
    );
    assert_eq!(
      RefPos::from_str(RefPos::Mercury.to_string().as_ref()),
      Ok(RefPos::Mercury)
    );
    assert_eq!(
      RefPos::from_str(RefPos::Venus.to_string().as_ref()),
      Ok(RefPos::Venus)
    );
    assert_eq!(
      RefPos::from_str(RefPos::Mars.to_string().as_ref()),
      Ok(RefPos::Mars)
    );
    assert_eq!(
      RefPos::from_str(RefPos::Jupiter.to_string().as_ref()),
      Ok(RefPos::Jupiter)
    );
    assert_eq!(
      RefPos::from_str(RefPos::Saturn.to_string().as_ref()),
      Ok(RefPos::Saturn)
    );
    assert_eq!(
      RefPos::from_str(RefPos::Uranus.to_string().as_ref()),
      Ok(RefPos::Uranus)
    );
    assert_eq!(
      RefPos::from_str(RefPos::Neptune.to_string().as_ref()),
      Ok(RefPos::Neptune)
    );
    assert_eq!(
      RefPos::from_str(RefPos::Pluto.to_string().as_ref()),
      Ok(RefPos::Pluto)
    );
    assert_eq!(
      RefPos::from_str(RefPos::UnknownRefPos.to_string().as_ref()),
      Ok(RefPos::UnknownRefPos)
    );
  }

  #[test]
  fn test_flavor_round_trip() {
    assert_eq!(
      Flavor::from_str(Flavor::Spher2.to_string().as_ref()),
      Ok(Flavor::Spher2)
    );
    assert_eq!(
      Flavor::from_str(Flavor::UnitSpher.to_string().as_ref()),
      Ok(Flavor::UnitSpher)
    );
    assert_eq!(
      Flavor::from_str(Flavor::Cart1.to_string().as_ref()),
      Ok(Flavor::Cart1)
    );
    assert_eq!(
      Flavor::from_str(Flavor::Cart2.to_string().as_ref()),
      Ok(Flavor::Cart2)
    );
    assert_eq!(
      Flavor::from_str(Flavor::Cart3.to_string().as_ref()),
      Ok(Flavor::Cart3)
    );
    assert_eq!(
      Flavor::from_str(Flavor::Spher3.to_string().as_ref()),
      Ok(Flavor::Spher3)
    );
  }

  #[test]
  fn test_unit_round_trip() {
    assert_eq!(
      SpaceUnit::from_str(SpaceUnit::Deg.to_string().as_ref()),
      Ok(SpaceUnit::Deg)
    );
    assert_eq!(
      SpaceUnit::from_str(SpaceUnit::Arcmin.to_string().as_ref()),
      Ok(SpaceUnit::Arcmin)
    );
    assert_eq!(
      SpaceUnit::from_str(SpaceUnit::Arcsec.to_string().as_ref()),
      Ok(SpaceUnit::Arcsec)
    );
    assert_eq!(
      SpaceUnit::from_str(SpaceUnit::M.to_string().as_ref()),
      Ok(SpaceUnit::M)
    );
    assert_eq!(
      SpaceUnit::from_str(SpaceUnit::Mm.to_string().as_ref()),
      Ok(SpaceUnit::Mm)
    );
    assert_eq!(
      SpaceUnit::from_str(SpaceUnit::Km.to_string().as_ref()),
      Ok(SpaceUnit::Km)
    );
    assert_eq!(
      SpaceUnit::from_str(SpaceUnit::AU.to_string().as_ref()),
      Ok(SpaceUnit::AU)
    );
    assert_eq!(
      SpaceUnit::from_str(SpaceUnit::Pc.to_string().as_ref()),
      Ok(SpaceUnit::Pc)
    );
    assert_eq!(
      SpaceUnit::from_str(SpaceUnit::Kpc.to_string().as_ref()),
      Ok(SpaceUnit::Kpc)
    );
    assert_eq!(
      SpaceUnit::from_str(SpaceUnit::Mpc.to_string().as_ref()),
      Ok(SpaceUnit::Mpc)
    );
  }
}
