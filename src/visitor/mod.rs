use std::error::Error;

use crate::{
  redshift::{RedshiftInterval, RedshiftValue},
  space::{
    common::{
      region::{BoxParams, CircleParams, ConvexParams, EllipseParams, PolygonParams},
      FillFrameRefposFlavor, FromPosToVelocity,
    },
    position::Position,
    positioninterval::PositionInterval,
  },
  spectral::{SpecInterval, SpecValue},
  time::{TimeElem, TimeIntervalArgs, TimeSimple, TimeStartArgs, TimeStopArgs},
};

pub mod impls;

pub struct StcVisitResult<T, S, P, R>
where
  T: TimeVisitor,
  S: SpaceVisitor,
  P: SpectralVisitor,
  R: RedshiftVisitor,
{
  pub time: Option<Result<T::Value, T::Error>>,
  pub space: Option<Result<S::Value, S::Error>>,
  pub spectral: Option<Result<P::Value, P::Error>>,
  pub redshift: Option<Result<R::Value, R::Error>>,
}
impl<T, S, P, R> StcVisitResult<T, S, P, R>
where
  T: TimeVisitor,
  S: SpaceVisitor,
  P: SpectralVisitor,
  R: RedshiftVisitor,
{
  pub(crate) fn new(
    time: Option<Result<T::Value, T::Error>>,
    space: Option<Result<S::Value, S::Error>>,
    spectral: Option<Result<P::Value, P::Error>>,
    redshift: Option<Result<R::Value, R::Error>>,
  ) -> Self {
    Self {
      time,
      space,
      spectral,
      redshift,
    }
  }
  pub fn time_res(&self) -> Option<&Result<T::Value, T::Error>> {
    self.time.as_ref()
  }
  pub fn space_res(&self) -> Option<&Result<S::Value, S::Error>> {
    self.space.as_ref()
  }
  pub fn spectral_res(&self) -> Option<&Result<P::Value, P::Error>> {
    self.spectral.as_ref()
  }
  pub fn redshift_res(&self) -> Option<&Result<R::Value, R::Error>> {
    self.redshift.as_ref()
  }
}

/// When visiting a single `Stc` structure, at most one of the four methods is called.
pub trait TimeVisitor: Sized {
  type Value;
  type Error: Error;

  fn visit_time_simple(self, time: &TimeSimple) -> Result<Self::Value, Self::Error>;

  fn visit_time_interval(
    self,
    time_interval: &TimeElem<TimeIntervalArgs>,
  ) -> Result<Self::Value, Self::Error>;

  fn visit_time_start(
    self,
    time_start: &TimeElem<TimeStartArgs>,
  ) -> Result<Self::Value, Self::Error>;

  fn visit_time_stop(self, time_stop: &TimeElem<TimeStopArgs>) -> Result<Self::Value, Self::Error>;
}

// We may build this visitor for elements in SpaceVisitor
// e.g. take into params convertsion of coordiantes to be applied everywhere, ...
// So thus Visitor is actually build from the SpaceVisitor
pub trait CompoundVisitor: Sized {
  /// Type of the value returned by an operation.
  type Value;
  /// Type of the returned error.
  type Error: Error;

  fn visit_allsky(&mut self) -> Result<Self::Value, Self::Error>;
  fn visit_circle(&mut self, circle: &CircleParams) -> Result<Self::Value, Self::Error>;
  fn visit_ellipse(&mut self, ellipse: &EllipseParams) -> Result<Self::Value, Self::Error>;
  fn visit_box(&mut self, skybox: &BoxParams) -> Result<Self::Value, Self::Error>;
  fn visit_polygon(&mut self, polygon: &PolygonParams) -> Result<Self::Value, Self::Error>;
  fn visit_convex(&mut self, convex: &ConvexParams) -> Result<Self::Value, Self::Error>;

  fn visit_not(&mut self, content_visit_result: Self::Value) -> Result<Self::Value, Self::Error>;
  fn visit_union(
    &mut self,
    content_visit_result: Vec<Self::Value>,
  ) -> Result<Self::Value, Self::Error>;
  fn visit_intersection(
    &mut self,
    content_visit_result: Vec<Self::Value>,
  ) -> Result<Self::Value, Self::Error>;
  fn visit_difference(
    &mut self,
    left_visit_result: Self::Value,
    right_visit_result: Self::Value,
  ) -> Result<Self::Value, Self::Error>;
}

/// When visiting a single `Stc` structure, at most one of the 12 methods is called.
pub trait SpaceVisitor: Sized {
  type Value;
  type Error: Error;
  type C: CompoundVisitor<Error = Self::Error>;

  fn new_compound_visitor(
    &self,
    fill_fram_refpos_flavor: &FillFrameRefposFlavor,
    from_pos_to_velocity: &FromPosToVelocity,
  ) -> Result<Self::C, Self::Error>;

  fn visit_position_simple(self, position: &Position) -> Result<Self::Value, Self::Error>;
  fn visit_position_interval(self, interval: &PositionInterval)
    -> Result<Self::Value, Self::Error>;

  fn visit_allsky(
    self,
    content_visit_result: <Self::C as CompoundVisitor>::Value,
  ) -> Result<Self::Value, Self::Error>;
  fn visit_circle(
    self,
    content_visit_result: <Self::C as CompoundVisitor>::Value,
  ) -> Result<Self::Value, Self::Error>;
  fn visit_ellipse(
    self,
    content_visit_result: <Self::C as CompoundVisitor>::Value,
  ) -> Result<Self::Value, Self::Error>;
  fn visit_box(
    self,
    content_visit_result: <Self::C as CompoundVisitor>::Value,
  ) -> Result<Self::Value, Self::Error>;
  fn visit_polygon(
    self,
    content_visit_result: <Self::C as CompoundVisitor>::Value,
  ) -> Result<Self::Value, Self::Error>;
  fn visit_convex(
    self,
    content_visit_result: <Self::C as CompoundVisitor>::Value,
  ) -> Result<Self::Value, Self::Error>;

  fn visit_not(
    self,
    content_visit_result: <Self::C as CompoundVisitor>::Value,
  ) -> Result<Self::Value, Self::Error>;
  fn visit_union(
    self,
    content_visit_result: <Self::C as CompoundVisitor>::Value,
  ) -> Result<Self::Value, Self::Error>;
  fn visit_intersection(
    self,
    content_visit_result: <Self::C as CompoundVisitor>::Value,
  ) -> Result<Self::Value, Self::Error>;
  fn visit_difference(
    self,
    content_visit_result: <Self::C as CompoundVisitor>::Value,
  ) -> Result<Self::Value, Self::Error>;
}

/*pub trait SpaceVisitor: Sized {
  type Value;
  type Error: Error;
  type C: CompoundVisitor<Value = Self::Value, Error = Self::Error>;

  fn new_compound_visitor(
    &self,
    fill_fram_refpos_flavor: &FillFrameRefposFlavor,
    from_pos_to_velocity: &FromPosToVelocity,
  ) -> Result<Self::C, Self::Error>;

  fn visit_position_simple(self, position: &Position) -> Result<Self::Value, Self::Error>;
  fn visit_position_interval(self, interval: &PositionInterval)
    -> Result<Self::Value, Self::Error>;

  fn visit_allsky(self, content_visit_result: Self::Value) -> Result<Self::Value, Self::Error>;
  fn visit_circle(self, content_visit_result: Self::Value) -> Result<Self::Value, Self::Error>;
  fn visit_ellipse(self, content_visit_result: Self::Value) -> Result<Self::Value, Self::Error>;
  fn visit_box(self, content_visit_result: Self::Value) -> Result<Self::Value, Self::Error>;
  fn visit_polygon(self, content_visit_result: Self::Value) -> Result<Self::Value, Self::Error>;
  fn visit_convex(self, content_visit_result: Self::Value) -> Result<Self::Value, Self::Error>;

  fn visit_not(self, content_visit_result: Self::Value) -> Result<Self::Value, Self::Error>;
  fn visit_union(self, content_visit_result: Self::Value) -> Result<Self::Value, Self::Error>;
  fn visit_intersection(
    self,
    content_visit_result: Self::Value,
  ) -> Result<Self::Value, Self::Error>;
  fn visit_difference(self, content_visit_result: Self::Value) -> Result<Self::Value, Self::Error>;
}*/

/// When visiting a single `Stc` structure, at most one of the two methods is called.
pub trait RedshiftVisitor: Sized {
  type Value;
  type Error: Error;

  fn visit_redshift_simple(self, redshift: &RedshiftValue) -> Result<Self::Value, Self::Error>;

  fn visit_redshift_interval(
    self,
    redshift_interval: &RedshiftInterval,
  ) -> Result<Self::Value, Self::Error>;
}

/// When visiting a single `Stc` structure, at most one of the two methods is called.
pub trait SpectralVisitor: Sized {
  type Value;
  type Error: Error;

  fn visit_spectral_simple(self, value: &SpecValue) -> Result<Self::Value, Self::Error>;

  fn visit_spectral_interval(self, interval: &SpecInterval) -> Result<Self::Value, Self::Error>;
}
