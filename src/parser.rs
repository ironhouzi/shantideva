#![allow(dead_code, unused_imports)]

use core::fmt;

#[path = "tibetan.rs"]
mod tibetan;

use tibetan::{Char, Cons, Syllable, TSHEG, Vowel};

use nom::{
    IResult,
    Parser,
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, multispace0, one_of},
    combinator::consumed,
    multi::{many_m_n, many1, fold_many0},
    sequence::tuple,
};

use tibetan::evaluate;

#[derive(Debug)]
enum Parsed {
    Passthrough(char),
    Translate(char),
    Text(Vec<Char>),
}

impl fmt::Display for Parsed {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Parsed::Passthrough(c) => write!(f, "{c}"),
            // TOOD: remove clone!
            Parsed::Text(s) => write!(f, "{}", evaluate(s.clone().into_iter())),
            Parsed::Translate(c) => {
                let r = match c {
                    ' ' => TSHEG,
                    _ => unimplemented!("No translatation for character: {c}")
                };
                write!(f, "{r}")
            },
        }
    }
}

fn vowel_parser(input: &str) -> IResult<&str, Char> {
    let (rest, vowel_char) = one_of("aeoui")(input)?;
    let vowel = match vowel_char {
        'a' => Char::Vowel(Vowel::A),
        'e' => Char::Vowel(Vowel::E),
        'o' => Char::Vowel(Vowel::O),
        'u' => Char::Vowel(Vowel::U),
        'i' => Char::Vowel(Vowel::I),
        _ => unreachable!("{vowel_char:?}")
    };
    Ok((rest, vowel))
}

// TODO: refactor using macro
fn parse_cons_ka(input: &str) -> IResult<&str, Char> {
    let (rest, _) = char('k')(input)?;
    Ok((rest, Char::Consonant(Cons::Ka)))
}

fn parse_cons_kha(input: &str) -> IResult<&str, Char> {
    let (rest, _) = tag("kh")(input)?;
    Ok((rest, Char::Consonant(Cons::Kha)))
}

fn parse_cons_ga(input: &str) -> IResult<&str, Char> {
    let (rest, _) = char('g')(input)?;
    Ok((rest, Char::Consonant(Cons::Ga)))
}

fn parse_cons_nga(input: &str) -> IResult<&str, Char> {
    let (rest, _) = tag("ng")(input)?;
    Ok((rest, Char::Consonant(Cons::Nga)))
}

fn parse_cons_ca(input: &str) -> IResult<&str, Char> {
    let (rest, _) = char('c')(input)?;
    Ok((rest, Char::Consonant(Cons::Ca)))
}

fn parse_cons_cha(input: &str) -> IResult<&str, Char> {
    let (rest, _) = tag("ch")(input)?;
    Ok((rest, Char::Consonant(Cons::Cha)))
}

fn parse_cons_ja(input: &str) -> IResult<&str, Char> {
    let (rest, _) = char('j')(input)?;
    Ok((rest, Char::Consonant(Cons::Ja)))
}

fn parse_cons_nya(input: &str) -> IResult<&str, Char> {
    let (rest, _) = tag("ny")(input)?;
    Ok((rest, Char::Consonant(Cons::Nya)))
}

fn parse_cons_ta(input: &str) -> IResult<&str, Char> {
    let (rest, _) = char('t')(input)?;
    Ok((rest, Char::Consonant(Cons::Ta)))
}

fn parse_cons_tha(input: &str) -> IResult<&str, Char> {
    let (rest, _) = tag("th")(input)?;
    Ok((rest, Char::Consonant(Cons::Tha)))
}

fn parse_cons_da(input: &str) -> IResult<&str, Char> {
    let (rest, _) = char('d')(input)?;
    Ok((rest, Char::Consonant(Cons::Da)))
}

fn parse_cons_na(input: &str) -> IResult<&str, Char> {
    let (rest, _) = char('n')(input)?;
    Ok((rest, Char::Consonant(Cons::Na)))
}

fn parse_cons_pa(input: &str) -> IResult<&str, Char> {
    let (rest, _) = char('p')(input)?;
    Ok((rest, Char::Consonant(Cons::Pa)))
}

fn parse_cons_pha(input: &str) -> IResult<&str, Char> {
    let (rest, _) = tag("ph")(input)?;
    Ok((rest, Char::Consonant(Cons::Pha)))
}

fn parse_cons_ba(input: &str) -> IResult<&str, Char> {
    let (rest, _) = char('b')(input)?;
    Ok((rest, Char::Consonant(Cons::Ba)))
}

fn parse_cons_ma(input: &str) -> IResult<&str, Char> {
    let (rest, _) = char('m')(input)?;
    Ok((rest, Char::Consonant(Cons::Ma)))
}

fn parse_cons_tsa(input: &str) -> IResult<&str, Char> {
    let (rest, _) = tag("ts")(input)?;
    Ok((rest, Char::Consonant(Cons::Tsa)))
}

fn parse_cons_tsha(input: &str) -> IResult<&str, Char> {
    let (rest, _) = tag("tsh")(input)?;
    Ok((rest, Char::Consonant(Cons::Tsha)))
}

fn parse_cons_dza(input: &str) -> IResult<&str, Char> {
    let (rest, _) = tag("dz")(input)?;
    Ok((rest, Char::Consonant(Cons::Dza)))
}

fn parse_cons_wa(input: &str) -> IResult<&str, Char> {
    let (rest, _) = char('w')(input)?;
    Ok((rest, Char::Consonant(Cons::Wa)))
}

fn parse_cons_zha(input: &str) -> IResult<&str, Char> {
    let (rest, _) = tag("zh")(input)?;
    Ok((rest, Char::Consonant(Cons::Zha)))
}

fn parse_cons_za(input: &str) -> IResult<&str, Char> {
    let (rest, _) = char('z')(input)?;
    Ok((rest, Char::Consonant(Cons::Za)))
}

fn parse_cons_achung(input: &str) -> IResult<&str, Char> {
    let (rest, _) = char('\'')(input)?;
    Ok((rest, Char::Consonant(Cons::Achung)))
}

fn parse_cons_ya(input: &str) -> IResult<&str, Char> {
    let (rest, _) = char('y')(input)?;
    Ok((rest, Char::Consonant(Cons::Ya)))
}

fn parse_cons_ra(input: &str) -> IResult<&str, Char> {
    let (rest, _) = char('r')(input)?;
    Ok((rest, Char::Consonant(Cons::Ra)))
}

fn parse_cons_la(input: &str) -> IResult<&str, Char> {
    let (rest, _) = char('l')(input)?;
    Ok((rest, Char::Consonant(Cons::La)))
}

fn parse_cons_sha(input: &str) -> IResult<&str, Char> {
    let (rest, _) = tag("sh")(input)?;
    Ok((rest, Char::Consonant(Cons::Sha)))
}

fn parse_cons_sa(input: &str) -> IResult<&str, Char> {
    let (rest, _) = char('s')(input)?;
    Ok((rest, Char::Consonant(Cons::Sa)))
}

fn parse_cons_ha(input: &str) -> IResult<&str, Char> {
    let (rest, _) = char('h')(input)?;
    Ok((rest, Char::Consonant(Cons::Ha)))
}

fn consonant_parser(input: &str) -> IResult<&str, Char> {
    let multi_char = alt((
        parse_cons_tsha,
        parse_cons_tsa,
        parse_cons_kha,
        parse_cons_nga,
        parse_cons_cha,
        parse_cons_nya,
        parse_cons_tha,
        parse_cons_pha,
        parse_cons_dza,
        parse_cons_zha,
        parse_cons_sha,
    ));

    let single_char = alt((
        parse_cons_ka,
        parse_cons_ga,
        parse_cons_ca,
        parse_cons_da,
        parse_cons_na,
        parse_cons_pa,
        parse_cons_ba,
        parse_cons_ta,
        parse_cons_ja,
        parse_cons_ma,
        parse_cons_achung,
        parse_cons_wa,
        parse_cons_za,
        parse_cons_ya,
        parse_cons_ra,
        parse_cons_la,
        parse_cons_sa,
        parse_cons_ha,
    ));

    alt((multi_char, single_char))(input)
}

fn parse_normal_syllable(input: &str) -> IResult<&str, Vec<Char>> {
    let (rest, (mut result, vowel, mut suffixes)) = tuple((
        many1(consonant_parser),
        vowel_parser,
        many_m_n(0, 2, consonant_parser,
    )))(input)?;

    result.push(vowel);
    result.append(&mut suffixes);
    Ok((rest, result))
}

fn parse_single_vowel(input: &str) -> IResult<&str, Vec<Char>> {
    let (rest, vowel) = vowel_parser(input)?;
    Ok((rest, vec![Char::Consonant(Cons::A), vowel]))
}

fn parse_syllable(input: &str) -> IResult<&str, Parsed> {
    let (rest, syllable) = alt((parse_single_vowel, parse_normal_syllable))(input)?;
    Ok((rest, Parsed::Text(syllable)))
}

fn passthrough(input: &str) -> IResult<&str, Parsed> {
    let (rest, byte) = one_of("\n\t")(input)?;
    Ok((rest, Parsed::Passthrough(byte)))
}

fn parse_symbols(input: &str) -> IResult<&str, Parsed> {
    let (rest, byte) = one_of(" ")(input)?;
    Ok((rest, Parsed::Translate(byte)))
}

pub fn parse(input: &str) -> IResult<&str, String> {
    fold_many0(
        alt((parse_symbols, passthrough, parse_syllable)),
        String::new,
        |mut acc: String, parsed| { acc.push_str(&parsed.to_string()); acc },
    )(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_text() {
        let (rest, text) = parse("sangs rgyas chos dang dge' dun").unwrap();
        assert_eq!(rest, "");
        assert_eq!(text, "སངས་རྒྱས་ཆོས་དང་དགེའ་དུན"); 
    }

    #[test]
    fn test_parse_prefix_root_subjoin_suffixes() {
        let (rest, skhyangs) = parse_syllable("skhyangs").unwrap();
        assert_eq!(rest, "");
        let Parsed::Text(s) = skhyangs else {panic!("Should be syllable!")};
        assert_eq!(s, vec![
            Char::Consonant(Cons::Sa),
            Char::Consonant(Cons::Kha),
            Char::Consonant(Cons::Ya),
            Char::Vowel(Vowel::A),
            Char::Consonant(Cons::Nga),
            Char::Consonant(Cons::Sa),
        ]);
    }

    #[test]
    fn test_parse_no_suffixes() {
        let (rest, skhya) = parse_syllable("skhya").unwrap();
        assert_eq!(rest, "");
        let Parsed::Text(s) = skhya else {panic!("Should be syllable!")};
        assert_eq!(s, vec![
            Char::Consonant(Cons::Sa),
            Char::Consonant(Cons::Kha),
            Char::Consonant(Cons::Ya),
            Char::Vowel(Vowel::A),
        ]);
    }

    #[test]
    fn test_parse_single_a() {
        let (_, single_a) = parse_single_vowel("a ").unwrap();
        assert_eq!(single_a, vec![Char::Consonant(Cons::A), Char::Vowel(Vowel::A)])
    }

    #[test]
    fn test_parse_single_e() {
        let (_, single_e) = parse_single_vowel("e ").unwrap();
        assert_eq!(single_e, vec![Char::Consonant(Cons::A), Char::Vowel(Vowel::E)])
    }
}
