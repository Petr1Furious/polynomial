use std::{fmt, ops};

pub struct Polynomial<Coef> {
    //
    coef: Vec<Coef>,
}

impl<Coef> fmt::Debug for Polynomial<Coef>
where
    Coef: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "coef: {:?}", self.coef)
    }
}

impl<T> Polynomial<T>
where
    T: Clone + Default + PartialEq,
{
    /// Create a zero polynomial
    pub fn zero() -> Self {
        Self { coef: Vec::new() }
    }

    /// Create a constant polynomial
    pub fn constant(c: T) -> Self {
        Self { coef: vec![c] }
    }
}

impl<Coef> From<Vec<Coef>> for Polynomial<Coef> {
    fn from(coef: Vec<Coef>) -> Self {
        Self { coef }
    }
}

impl<Coef> From<Coef> for Polynomial<Coef> {
    fn from(coef: Coef) -> Self {
        Self { coef: vec![coef] }
    }
}

impl<Coef> ops::Add for Polynomial<Coef>
where
    Coef: ops::Add<Output = Coef> + Default,
{
    type Output = Self;

    fn add(self, other: Self) -> Self {
        let mut coef = Vec::new();
        let mut self_iter = self.coef.into_iter();
        let mut other_iter = other.coef.into_iter();
        loop {
            match (self_iter.next(), other_iter.next()) {
                (Some(a), Some(b)) => coef.push(a + b),
                (Some(a), None) => coef.push(a),
                (None, Some(b)) => coef.push(b),
                (None, None) => break,
            }
        }
        Self { coef }
    }
}

impl<Coef> ops::Add<Coef> for Polynomial<Coef>
where
    Coef: ops::Add<Output = Coef> + Default,
{
    type Output = Self;

    fn add(self, other: Coef) -> Self {
        self + Self::from(other)
    }
}

impl<Coef> Default for Polynomial<Coef>
where
    Coef: Default,
{
    fn default() -> Self {
        Self { coef: Vec::new() }
    }
}

impl<Coef> ops::Sub<Coef> for Polynomial<Coef>
where
    Coef: ops::Add<Output = Coef> + ops::Neg<Output = Coef> + Default,
{
    type Output = Self;

    fn sub(self, other: Coef) -> Self {
        self + Self::from(-other)
    }
}

impl<Coef> ops::Neg for Polynomial<Coef>
where
    Coef: ops::Neg<Output = Coef>,
{
    type Output = Self;

    fn neg(self) -> Self {
        Self {
            coef: self.coef.into_iter().map(|c| -c).collect(),
        }
    }
}

impl<Coef> ops::Sub for Polynomial<Coef>
where
    Coef: ops::Add<Output = Coef> + ops::Neg<Output = Coef> + Default,
{
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        self + (-other)
    }
}

impl<Coef> ops::Mul<Coef> for Polynomial<Coef>
where
    Coef: ops::Mul<Output = Coef> + Clone + Default,
{
    type Output = Self;

    fn mul(self, other: Coef) -> Self {
        Self {
            coef: self.coef.into_iter().map(|c| c * other.clone()).collect(),
        }
    }
}

impl<Coef> ops::Mul for Polynomial<Coef>
where
    Coef: ops::Add<Output = Coef> + ops::Mul<Output = Coef> + Clone + Default,
{
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        let mut coef = vec![Coef::default(); self.coef.len() + other.coef.len() - 1];
        for (i, a) in self.coef.iter().enumerate() {
            for (j, b) in other.coef.iter().enumerate() {
                coef[i + j] = coef[i + j].clone() + a.clone() * b.clone();
            }
        }
        Self { coef }
    }
}

pub trait One {
    fn one() -> Self;
}

impl<Coef> One for Polynomial<Coef>
where
    Coef: One,
{
    fn one() -> Self {
        Self::from(Coef::one())
    }
}

impl One for i32 {
    fn one() -> Self {
        1
    }
}

impl One for f64 {
    fn one() -> Self {
        1.0
    }
}

impl<Coef> Clone for Polynomial<Coef>
where
    Coef: Clone,
{
    fn clone(&self) -> Self {
        Self {
            coef: self.coef.clone(),
        }
    }
}

impl<Coef> Polynomial<Coef> {
    /// Raise polynomial to the n-th power
    pub fn pow(mut self, mut n: u64) -> Self
    where
        Coef: One + ops::Add<Output = Coef> + ops::Mul<Output = Coef> + Clone + Default,
    {
        let mut result = Self::one();
        while n > 0 {
            if n & 1 == 1 {
                result = result * self.clone();
            }
            n >>= 1;
            if n > 0 {
                self = self.clone() * self.clone();
            }
        }
        result
    }

    /// Evaluate polynomial at given value
    pub fn eval(&self, x: Coef) -> Coef
    where
        Coef: ops::Add<Output = Coef> + ops::Mul<Output = Coef> + Clone + Default,
    {
        self.coef
            .iter()
            .rev()
            .fold(Coef::default(), |acc, c| acc * x.clone() + c.clone())
    }

    /// Substitute polynomial into another polynomial
    pub fn substitute(&self, other: &Self) -> Self
    where
        Coef: One + ops::Add<Output = Coef> + ops::Mul<Output = Coef> + Clone + Default,
    {
        self.coef
            .iter()
            .enumerate()
            .fold(Self::default(), |acc, (i, c)| {
                acc + Polynomial::from(c.clone()) * other.clone().pow(i as u64)
            })
    }

    /// Remove trailing zero coefficients
    pub fn trim(&mut self)
    where
        Coef: PartialEq + Default,
    {
        while let Some(c) = self.coef.last() {
            if c == &Coef::default() {
                self.coef.pop();
            } else {
                break;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add() {
        let a: Polynomial<i32> = Polynomial::from(vec![1, 2, 3]);
        let b: Polynomial<i32> = Polynomial::from(vec![4, 5, 6]);
        let c = a + b;
        assert_eq!(c.coef, vec![5, 7, 9]);
    }

    #[test]
    fn test_neg() {
        let a = Polynomial::<i32>::from(vec![1, 2, 3]);
        let b = -a;
        assert_eq!(b.coef, vec![-1, -2, -3]);
    }

    #[test]
    fn test_sub() {
        let a = Polynomial::<i32>::from(vec![1, 2, 3]);
        let b = Polynomial::<i32>::from(vec![4, 5, 6]);
        let c = a - b;
        assert_eq!(c.coef, vec![-3, -3, -3]);
    }

    #[test]
    fn test_add_constant() {
        let p = Polynomial::from(vec![1, 2, 3]);
        let q = p + 4;
        assert_eq!(q.coef, vec![5, 2, 3]);
        let r = q + Polynomial::constant(4);
        assert_eq!(r.coef, vec![9, 2, 3]);
    }

    #[test]
    fn test_sub_constant() {
        let p = Polynomial::<i32>::from(vec![1, 2, 3]);
        let q = p - 4;
        assert_eq!(q.coef, vec![-3, 2, 3]);
    }

    #[test]
    fn test_mul() {
        let a = Polynomial::<i32>::from(vec![1, 2, 3]);
        let b = Polynomial::<i32>::from(vec![4, 5, 6]);
        let c = a * b;
        assert_eq!(c.coef, vec![4, 13, 28, 27, 18]);
    }

    #[test]
    fn test_pow() {
        let a = Polynomial::<i32>::from(vec![1, 2, 3]);
        let b = a.pow(3);
        assert_eq!(b.coef, vec![1, 6, 21, 44, 63, 54, 27]);
    }

    #[test]
    fn test_bin_pow() {
        let a = Polynomial::<i32>::from(2);
        let b = a.pow(30);
        assert_eq!(b.coef, vec![1 << 30]);
    }

    #[test]
    fn test_eval() {
        let a = Polynomial::<i32>::from(vec![1, 2, 3]);
        let b = a.eval(2);
        assert_eq!(b, 17);
    }

    #[test]
    fn test_substitute() {
        let p = Polynomial::<i32>::from(vec![1, 1]); // 1 + x
        let q = Polynomial::<i32>::from(vec![2]); // 2
        let r = p.substitute(&q); // 1 + 2 = 3
        assert_eq!(r.coef, vec![3]);
    }

    #[test]
    fn test_trim() {
        let mut a = Polynomial::<i32>::from(vec![1, 2, 3, 0, 0]);
        a.trim();
        assert_eq!(a.coef, vec![1, 2, 3]);
    }

    #[derive(Clone, PartialEq)]
    struct NaturalNumber(u64);

    impl Default for NaturalNumber {
        fn default() -> Self {
            Self(0)
        }
    }

    impl fmt::Debug for NaturalNumber {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "{}", self.0)
        }
    }

    impl One for NaturalNumber {
        fn one() -> Self {
            Self(1)
        }
    }

    impl ops::Add for NaturalNumber {
        type Output = Self;

        fn add(self, other: Self) -> Self {
            Self(self.0 + other.0)
        }
    }

    impl ops::Mul for NaturalNumber {
        type Output = Self;

        fn mul(self, other: Self) -> Self {
            Self(self.0 * other.0)
        }
    }

    #[test]
    fn test_custom_type() {
        let a = Polynomial::<NaturalNumber>::from(vec![
            NaturalNumber(1),
            NaturalNumber(2),
            NaturalNumber(3),
        ]);
        let b = a.clone().pow(2);
        assert_eq!(
            b.coef,
            vec![
                NaturalNumber(1),
                NaturalNumber(4),
                NaturalNumber(10),
                NaturalNumber(12),
                NaturalNumber(9)
            ]
        );
        let c = a.clone().eval(NaturalNumber(2));
        assert_eq!(c, NaturalNumber(17));
        // let d = -a.clone(); // This should not compile
    }
}
