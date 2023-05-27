#![allow(dead_code)]

use core::fmt;

use itertools::{Itertools, TupleWindows}; // 0.10.0

pub const TSHEG: char = '\u{0f0b}';

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Cons {
    Ka,
    Kha,
    Ga,
    Nga,
    Ca,
    Cha,
    Ja,
    Nya,
    Ta,
    Tha,
    Da,
    Na,
    Pa,
    Pha,
    Ba,
    Ma,
    Tsa,
    Tsha,
    Dza,
    Wa,
    Zha,
    Za,
    Achung,
    Ya,
    Ra,
    La,
    Sha,
    Sa,
    Ha,
    A,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Vowel {
    I,
    U,
    E,
    O,
    A,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Prefix {
    cons: Cons,
}

impl From<Vowel> for char {
    fn from(value: Vowel) -> Self {
        match value {
            Vowel::I => '\u{0f72}',
            Vowel::U => '\u{0f74}',
            Vowel::E => '\u{0f7a}',
            Vowel::O => '\u{0f7c}',
            Vowel::A => '\u{0f68}',
        }
    }
}


impl From<Cons> for char {
    fn from(value: Cons) -> Self {
        match value {
            Cons::Ka => '\u{0f40}',
            Cons::Kha => '\u{0f41}',
            Cons::Ga => '\u{0f42}',
            Cons::Nga => '\u{0f44}',
            Cons::Ca => '\u{0f45}',
            Cons::Cha => '\u{0f46}',
            Cons::Ja => '\u{0f47}',
            Cons::Nya => '\u{0f49}',
            Cons::Ta => '\u{0f4f}',
            Cons::Tha => '\u{0f50}',
            Cons::Da => '\u{0f51}',
            Cons::Na => '\u{0f53}',
            Cons::Pa => '\u{0f54}',
            Cons::Pha => '\u{0f55}',
            Cons::Ba => '\u{0f56}',
            Cons::Ma => '\u{0f58}',
            Cons::Tsa => '\u{0f59}',
            Cons::Tsha => '\u{0f5a}',
            Cons::Dza => '\u{0f5b}',
            Cons::Wa => '\u{0f5d}',
            Cons::Zha => '\u{0f5e}',
            Cons::Za => '\u{0f5f}',
            Cons::Achung => '\u{0f60}',
            Cons::Ya => '\u{0f61}',
            Cons::Ra => '\u{0f62}',
            Cons::La => '\u{0f63}',
            Cons::Sha => '\u{0f64}',
            Cons::Sa => '\u{0f66}',
            Cons::Ha => '\u{0f67}',
            Cons::A => '\u{0f68}',
        }
    }
}

impl From<Cons> for Prefix {
    fn from(cons: Cons) -> Self {
        Self { cons }
    }
}

impl TryFrom<Option<Cons>> for Prefix {
    type Error = ();

    fn try_from(cons: Option<Cons>) -> Result<Self, Self::Error> {
        if let Some(cons) = cons {
            match cons {
                Cons::Ga | Cons::Da | Cons::Ba | Cons::Ma | Cons::Achung => Ok(Prefix { cons }),
                _ => Err(()),
            }
        } else {
            Err(())
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Suffix {
    cons: Cons,
}

impl From<Cons> for Suffix {
    fn from(cons: Cons) -> Self {
        Self { cons }
    }
}

impl TryFrom<Option<Cons>> for Suffix {
    type Error = ();

    fn try_from(cons: Option<Cons>) -> Result<Self, Self::Error> {
        if let Some(cons) = cons {
            match cons {
                Cons::Ga
                | Cons::Nga
                | Cons::Da
                | Cons::Na
                | Cons::Ba
                | Cons::Ma
                | Cons::Achung
                | Cons::Ra
                | Cons::La
                | Cons::Sa => Ok(Suffix { cons }),
                _ => {
                    Err(())
                },
            }
        } else {
            Err(())
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Suffix2 {
    cons: Cons,
}

impl From<Cons> for Suffix2 {
    fn from(cons: Cons) -> Self {
        Self { cons }
    }
}

impl TryFrom<Option<Cons>> for Suffix2 {
    type Error = ();

    fn try_from(cons: Option<Cons>) -> Result<Self, Self::Error> {
        if let Some(cons) = cons {
            match cons { 
                Cons::Da | Cons::Sa => Ok(Suffix2 { cons }),
                _ => Err(()),
            }
        } else {
            Err(())
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Superscribe {
    cons: Cons,
}

impl From<Cons> for Superscribe {
    fn from(cons: Cons) -> Self {
        Self { cons }
    }
}

impl TryFrom<(Option<Cons>, Cons)> for Superscribe {
    type Error = ();

    fn try_from(letters: (Option<Cons>, Cons)) -> Result<Self, Self::Error> {
        match letters {
            (
                Some(Cons::Ra),
                Cons::Ka
                | Cons::Ga
                | Cons::Nga
                | Cons::Ja
                | Cons::Nya
                | Cons::Ta
                | Cons::Da
                | Cons::Na
                | Cons::Ba
                | Cons::Ma
                | Cons::Tsa
                | Cons::Dza,
            ) => Ok(Superscribe::from(Cons::Ra)),
            (
                Some(Cons::La),
                Cons::Ka
                | Cons::Ga
                | Cons::Nga
                | Cons::Ca
                | Cons::Ja
                | Cons::Ta
                | Cons::Da
                | Cons::Pa
                | Cons::Ba
                | Cons::Ha,
            ) => Ok(Superscribe::from(Cons::La)),
            (
                Some(Cons::Sa),
                Cons::Ka
                | Cons::Ga
                | Cons::Nga
                | Cons::Nya
                | Cons::Ta
                | Cons::Da
                | Cons::Na
                | Cons::Pa
                | Cons::Ba
                | Cons::Ma
                | Cons::Tsa,
            ) => Ok(Superscribe::from(Cons::Sa)),
            (_, _) => Err(()),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Subjoin {
    cons: Cons,
}

impl From<Cons> for Subjoin {
    fn from(cons: Cons) -> Self {
        Self { cons }
    }
}

impl Into<Cons> for &Subjoin {
    fn into(self) -> Cons {
        self.cons
    }
}

impl Into<Cons> for Subjoin {
    fn into(self) -> Cons {
        self.cons
    }
}

impl TryFrom<(Option<Cons>, Cons)> for Subjoin {
    type Error = ();

    fn try_from(letters: (Option<Cons>, Cons)) -> Result<Self, Self::Error> {
        let (root, subjoin) = letters;
        if let Some(root) = root {
            match (subjoin, root) {
                (
                    Cons::Wa,
                    Cons::Ka
                    | Cons::Kha
                    | Cons::Ga
                    | Cons::Ca
                    | Cons::Nya
                    | Cons::Ta
                    | Cons::Da
                    | Cons::Tsa
                    | Cons::Tsha
                    | Cons::Zha
                    | Cons::Za
                    | Cons::Ra
                    | Cons::La
                    | Cons::Sha
                    | Cons::Sa
                    | Cons::Ha,
                ) => Ok(Subjoin { cons: Cons::Wa }),
                (
                    Cons::Ya,
                    Cons::Ka
                    | Cons::Kha
                    | Cons::Ga
                    | Cons::Pa
                    | Cons::Pha
                    | Cons::Ba
                    | Cons::Ma
                    | Cons::Ha,
                ) => Ok(Subjoin { cons: Cons::Ya }),
                (
                    Cons::Ra,
                    Cons::Ka
                    | Cons::Kha
                    | Cons::Ga
                    | Cons::Ta
                    | Cons::Tha
                    | Cons::Da
                    | Cons::Na
                    | Cons::Pa
                    | Cons::Pha
                    | Cons::Ba
                    | Cons::Ma
                    | Cons::Sa
                    | Cons::Ha,
                ) => Ok(Subjoin { cons: Cons::Ra }),
                (Cons::La, Cons::Ka | Cons::Ga | Cons::Ba | Cons::Ra | Cons::Sa | Cons::Za) => {
                    Ok(Subjoin { cons: Cons::La })
                }
                (_, _) => Err(()),
            }
        } else {
            Err(())
        }
    }
}

#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct Syllable {
    pub prefix: Option<Prefix>,
    pub superscribe: Option<Superscribe>,
    pub root: Option<Cons>,
    pub subjoin: Option<Subjoin>,
    pub subjoin2: Option<Subjoin>,
    pub vowel: Option<Vowel>,
    pub suffix: Option<Suffix>,
    pub suffix2: Option<Suffix2>,
}

impl fmt::Display for Syllable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut i: u8 = 0;
        let mut unicode: String = "".to_string();
        let mut head_set = false;

        if let Some(c) = self.prefix.map(|c| c.cons.into()) {
            unicode.push(c);
        }

        loop {
            let chr: Option<char> = match i {
                0 => self.superscribe.map(|c| c.cons.into()),
                1 => self.root.map(|x| x.into()),
                2 => self.subjoin.map(|x| x.cons.into()),
                3 => self.subjoin2.map(|x| x.cons.into()),
                4 => self.vowel.map(|x| x.into()),
                5 => self.suffix.map(|x| x.cons.into()),
                6 => self.suffix2.map(|x| x.cons.into()),
                _ => break
            };
            if let Some(c) = chr {
                if head_set && i < 4 {
                    let Some(stacked_char) = char::from_u32(c as u32 + 0x50) else {
                        unreachable!("Add 0x50 to {c:?} overflow!");
                    };
                    unicode.push(stacked_char);
                } else {
                    if i != 4 || c != char::from(Vowel::A) {
                        unicode.push(c);
                    }
                }
                head_set = true
            }
            i += 1;
        }
        write!(f, "{unicode}")
    }
}

impl Syllable {
    pub fn add(self, letter: Char) -> Self {
        if self.vowel.is_some() {
            let new: Syllable = match (self.suffix, self.suffix2, letter) {
                (None, None, Char::Consonant(cons)) => {
                    let Ok(suffix) = Suffix::try_from(Some(cons)) else {
                        panic!("Invalid Suffix")
                    };
                    Syllable {
                        suffix: Some(suffix),
                        ..self
                    }
                }
                (Some(_), None, Char::Consonant(cons)) => {
                    let Ok(suffix2) = Suffix2::try_from(Some(cons)) else {
                        panic!("Invalid second suffix!")
                    };
                    Syllable {
                        suffix2: Some(suffix2),
                        ..self
                    }
                }
                (_, _, _) => panic!("Suffix error!"),
            };
            return new;
        }
        match (&self.prevowel_count(), letter) {
            (_, Char::Vowel(v)) => Syllable {
                vowel: Some(v),
                ..self
            },
            (0, Char::Consonant(c)) => Syllable {
                root: Some(c),
                ..Syllable::default()
            },
            (1, Char::Consonant(c)) => {
                if let Ok(cons) = Subjoin::try_from((self.root, c)) {
                    return Syllable {
                        subjoin: Some(cons),
                        root: self.root,
                        ..Syllable::default()
                    };
                };
                if let Ok(cons) = Superscribe::try_from((self.root, c)) {
                    return Syllable {
                        superscribe: Some(cons),
                        root: Some(c),
                        ..Syllable::default()
                    };
                };
                if let Ok(cons) = Prefix::try_from(self.root) {
                    return Syllable {
                        prefix: Some(cons),
                        root: Some(c),
                        ..Syllable::default()
                    };
                };
                panic!("Could not add consonant {:?} to {:?}", c, self);
            }
            (2, Char::Consonant(c)) => {
                if let Ok(subjoin) = Subjoin::try_from((self.root, c)) {
                    if (self.prefix.is_some() || self.root.is_some()) && self.subjoin.is_none() {
                        return Syllable {
                            subjoin: Some(subjoin),
                            ..self
                        };
                    }
                    if subjoin.cons == Cons::Wa {
                        // special case
                        assert_eq!(self.subjoin, Some(Subjoin { cons: Cons::Ra }));
                        return Syllable {
                            subjoin2: Some(subjoin),
                            ..self
                        };
                    };
                    panic!("Could not add {subjoin:#?} to {self:#?}!");
                };
                if let Ok(superscribe) = Superscribe::try_from((self.root, c)) {
                    return Syllable {
                        superscribe: Some(superscribe),
                        root: Some(c),
                        ..self
                    };
                }
                if let Some(subjoin) = &self.subjoin {
                    if let Ok(old_subjoin) = Superscribe::try_from((Some(subjoin.into()), c)) {
                        if let Ok(old_root) = Prefix::try_from(self.root) {
                            return Syllable {
                                prefix: Some(old_root),
                                superscribe: Some(old_subjoin),
                                root: Some(c),
                                subjoin: None,
                                ..self
                            };
                        }
                    }
                }
                unimplemented!("Could not add valid {c:#?} to {self:#?}!");
            }
            (_, Char::Consonant(_)) => self,
        }
    }
    pub fn prevowel_count(&self) -> usize {
        let mut result = 0;
        if let Some(_) = self.prefix {
            result += 1;
        }
        if let Some(_) = self.superscribe {
            result += 1;
        }
        if let Some(_) = self.root {
            result += 1;
        }
        if let Some(_) = self.subjoin {
            result += 1;
        }
        if let Some(_) = self.subjoin2 {
            result += 1;
        }
        result
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Char {
    Consonant(Cons),
    Vowel(Vowel),
}

pub fn evaluate(sequence: impl Iterator<Item = Char> + std::fmt::Debug) -> Syllable {
    // this is convoluted due to .take_while() slurping up the vowel!
    let window_iter: TupleWindows<_, (Char, Char)> = sequence.tuple_windows();
    let mut char = None;

    let result = window_iter.fold(Syllable::default(), |syllable, chars| match chars {
        (Char::Consonant(c), c2) => {
            char = Some(c2);
            syllable.add(Char::Consonant(c))
        }
        (Char::Vowel(v), Char::Consonant(c)) => {
            char = Some(Char::Consonant(c));
            syllable.add(Char::Vowel(v))
        }
        (Char::Vowel(_), Char::Vowel(_)) => unreachable!("Two vowels")
    });

    // TODO: this is ugly
    let Some(char) = char else {
        panic!("No second char2!")
    };
    result.add(char)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_prefix() {
        let syllable = [
            Char::Consonant(Cons::Ma),
            Char::Consonant(Cons::Ga),
            Char::Vowel(Vowel::O),
        ];
        assert_eq!(
            evaluate(syllable.into_iter()),
            Syllable {
                prefix: Some(Prefix { cons: Cons::Ma }),
                root: Some(Cons::Ga),
                vowel: Some(Vowel::O),
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_superscribe() {
        let syllable = [
            Char::Consonant(Cons::Ra),
            Char::Consonant(Cons::Ta),
            Char::Vowel(Vowel::A),
        ];

        assert_eq!(
            evaluate(syllable.into_iter()),
            Syllable {
                root: Some(Cons::Ta),
                vowel: Some(Vowel::A),
                superscribe: Some(Superscribe::from(Cons::Ra)),
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_subjoin() {
        let syllable = [
            Char::Consonant(Cons::Ga),
            Char::Consonant(Cons::Ya),
            Char::Vowel(Vowel::A),
        ];

        assert_eq!(
            evaluate(syllable.into_iter()),
            Syllable {
                root: Some(Cons::Ga),
                vowel: Some(Vowel::A),
                subjoin: Some(Subjoin::from(Cons::Ya)),
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_root_subjoin() {
        let syllable = [
            Char::Consonant(Cons::Ga),
            Char::Consonant(Cons::Ra),
            Char::Vowel(Vowel::A),
        ];

        assert_eq!(
            evaluate(syllable.into_iter()),
            Syllable {
                root: Some(Cons::Ga),
                vowel: Some(Vowel::A),
                subjoin: Some(Subjoin::from(Cons::Ra)),
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_rata_wasur() {
        let syllable = [
            Char::Consonant(Cons::Ga),
            Char::Consonant(Cons::Ra),
            Char::Consonant(Cons::Wa),
            Char::Vowel(Vowel::A),
        ];

        assert_eq!(
            evaluate(syllable.into_iter()),
            Syllable {
                root: Some(Cons::Ga),
                vowel: Some(Vowel::A),
                subjoin: Some(Subjoin::from(Cons::Ra)),
                subjoin2: Some(Subjoin::from(Cons::Wa)),
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_prefix_superscribe_root() {
        let syllable = [
            Char::Consonant(Cons::Achung),
            Char::Consonant(Cons::Ra),
            Char::Consonant(Cons::Ba),
            Char::Vowel(Vowel::A),
        ];

        assert_eq!(
            evaluate(syllable.into_iter()),
            Syllable {
                prefix: Some(Prefix::from(Cons::Achung)),
                root: Some(Cons::Ba),
                vowel: Some(Vowel::A),
                superscribe: Some(Superscribe::from(Cons::Ra)),
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_prefix_superscribe_root2() {
        let syllable = [
            Char::Consonant(Cons::Ba),
            Char::Consonant(Cons::Ra),
            Char::Consonant(Cons::Nya),
            Char::Vowel(Vowel::E),
        ];

        assert_eq!(
            evaluate(syllable.into_iter()),
            Syllable {
                prefix: Some(Prefix::from(Cons::Ba)),
                root: Some(Cons::Nya),
                vowel: Some(Vowel::E),
                superscribe: Some(Superscribe::from(Cons::Ra)),
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_prefix_root_subjoin() {
        let syllable = [
            Char::Consonant(Cons::Achung),
            Char::Consonant(Cons::Ga),
            Char::Consonant(Cons::Ya),
            Char::Vowel(Vowel::A),
        ];

        assert_eq!(
            evaluate(syllable.into_iter()),
            Syllable {
                prefix: Some(Prefix::from(Cons::Achung)),
                root: Some(Cons::Ga),
                vowel: Some(Vowel::A),
                subjoin: Some(Subjoin::from(Cons::Ya)),
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_subscript_root_subjoin() {
        let syllable = [
            Char::Consonant(Cons::Sa),
            Char::Consonant(Cons::Pa),
            Char::Consonant(Cons::Ya),
            Char::Vowel(Vowel::I),
        ];

        assert_eq!(
            evaluate(syllable.into_iter()),
            Syllable {
                root: Some(Cons::Pa),
                vowel: Some(Vowel::I),
                superscribe: Some(Superscribe::from(Cons::Sa)),
                subjoin: Some(Subjoin::from(Cons::Ya)),
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_suffix() {
        let syllable = [
            Char::Consonant(Cons::Sa),
            Char::Vowel(Vowel::A),
            Char::Consonant(Cons::Nga),
            Char::Consonant(Cons::Sa),
        ];

        assert_eq!(
            evaluate(syllable.into_iter()),
            Syllable {
                root: Some(Cons::Sa),
                vowel: Some(Vowel::A),
                suffix: Some(Suffix::from(Cons::Nga)),
                suffix2: Some(Suffix2::from(Cons::Sa)),
                ..Syllable::default()
            }
        );
    }

    #[test]
    fn test_syllable_iterator() {
        let syllable = [
            Char::Consonant(Cons::Sa),
            Char::Consonant(Cons::Ka),
            Char::Consonant(Cons::Ya),
            Char::Vowel(Vowel::A),
            Char::Consonant(Cons::Nga),
            Char::Consonant(Cons::Sa),
        ];

        let skyangs = evaluate(syllable.into_iter());

        assert_eq!(
            skyangs,
            Syllable {
                prefix: None,
                superscribe: Some(Superscribe::from(Cons::Sa)),
                subjoin: Some(Subjoin::from(Cons::Ya)),
                subjoin2: None,
                root: Some(Cons::Ka),
                vowel: Some(Vowel::A),
                suffix: Some(Suffix::from(Cons::Nga)),
                suffix2: Some(Suffix2::from(Cons::Sa)),
            }
        );

        assert_eq!(skyangs.to_string(), "ས\u{f90}\u{fb1}ངས");
    }

    #[test]
    fn test_singel_vowel_a() {
        let syllable = [
            Char::Consonant(Cons::A),
            Char::Vowel(Vowel::A),
        ];

        let a_syllable = evaluate(syllable.into_iter());

        assert_eq!(
            a_syllable,
            Syllable {
                root: Some(Cons::A),
                vowel: Some(Vowel::A),
                ..Default::default()
            }
        );

        assert_eq!(a_syllable.to_string(), "ཨ");
    }

    #[test]
    fn test_singel_vowel_e() {
        let syllable = [
            Char::Consonant(Cons::A),
            Char::Vowel(Vowel::E),
        ];

        let a_syllable = evaluate(syllable.into_iter());

        assert_eq!(
            a_syllable,
            Syllable {
                root: Some(Cons::A),
                vowel: Some(Vowel::E),
                ..Default::default()
            }
        );

        assert_eq!(a_syllable.to_string(), "ཨ\u{f7a}");
    }

    #[test]
    #[should_panic]
    fn test_invalid_suffix() {
        let syllable = [
            Char::Consonant(Cons::Sa),
            Char::Vowel(Vowel::A),
            Char::Consonant(Cons::Ka),
        ];

        evaluate(syllable.into_iter());
    }
}
