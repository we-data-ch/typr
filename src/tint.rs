use std::ops::Add;
use std::ops::Sub;
use std::ops::Mul;
use std::ops::Div;
use serde::Serialize;
use std::fmt;

#[derive(Debug, Clone, Copy, Serialize, PartialEq, Eq, Hash)]
pub enum Tint {
    Val(i32),
    Unknown
}

impl fmt::Display for Tint {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Tint::Val(i) => write!(f, "{}", i),
            _ => write!(f, "{}", "int")
        }
    }
}

impl Add for Tint {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        match (self, other) {
            (Tint::Val(i1), Tint::Val(i2)) => Tint::Val(i1+i2),
            (Tint::Unknown, t) => panic!("Can't add opaque int type with {:?}", t),
            (t, Tint::Unknown) => panic!("Can't add {:?} with opaque int type", t)
        }
    }
}

impl Sub for Tint {
    type Output = Self;
    fn sub(self, other: Self) -> Self {
        match (self, other) {
            (Tint::Val(i1), Tint::Val(i2)) => Tint::Val(i1-i2),
            (Tint::Unknown, t) => panic!("Can't substract opaque int type with {:?}", t),
            (t, Tint::Unknown) => panic!("Can't substract {:?} with opaque int type", t)
        }
    }
}

impl Mul for Tint {
    type Output = Self;
    fn mul(self, other: Self) -> Self {
        match (self, other) {
            (Tint::Val(i1), Tint::Val(i2)) => Tint::Val(i1*i2),
            (Tint::Unknown, t) => panic!("Can't substract opaque int type with {:?}", t),
            (t, Tint::Unknown) => panic!("Can't substract {:?} with opaque int type", t)
        }
    }
}

impl Div for Tint {
    type Output = Self;
    fn div(self, other: Self) -> Self {
        match (self, other) {
            (Tint::Val(i1), Tint::Val(i2)) => Tint::Val(i1/i2),
            (Tint::Unknown, t) => panic!("Can't substract opaque int type with {:?}", t),
            (t, Tint::Unknown) => panic!("Can't substract {:?} with opaque int type", t)
        }
    }
}

impl From<i32> for  Tint {
   fn from(val: i32) -> Self {
        Tint::Val(val)
   } 
}

impl From<u32> for  Tint {
   fn from(val: u32) -> Self {
        Tint::Val(val as i32)
   } 
}

impl From<usize> for  Tint {
   fn from(val: usize) -> Self {
        Tint::Val(val as i32)
   } 
}

impl From<&str> for  Tint {
   fn from(val: &str) -> Self {
        if val == "" {
           Tint::Unknown 
        } else {
            Tint::Val(val.parse::<i32>().unwrap_or(0))
        }
   } 
}

impl From<Tint> for u32 {
   fn from(val: Tint) -> Self {
       match val {
           Tint::Val(i) => i as u32,
           Tint::Unknown => 0 as u32
       }
   } 
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tint_val(){
        assert_eq!(Tint::Val(2), Tint::Val(3));
    }
}
