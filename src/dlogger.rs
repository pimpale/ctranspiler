use super::token::TokenKind;
use lsp_types::Diagnostic;
use lsp_types::DiagnosticRelatedInformation;
use lsp_types::DiagnosticSeverity;
use lsp_types::Location;
use lsp_types::NumberOrString;
use lsp_types::Range;
use lsp_types::Uri;
use num_bigint::BigInt;
use std::str::FromStr;
use std::sync::mpsc::channel;
use std::sync::mpsc::Receiver;
use std::sync::mpsc::Sender;

pub struct DiagnosticLog {
    recv: Receiver<Diagnostic>,
    send: Sender<Diagnostic>,
}

impl DiagnosticLog {
    pub fn new() -> Self {
        let (send, recv) = channel();
        DiagnosticLog { recv, send }
    }

    pub fn get_logger(&mut self, source: Option<String>) -> DiagnosticLogger {
        DiagnosticLogger {
            sender: self.send.clone(),
            source,
        }
    }
}

pub struct DiagnosticLogger {
    sender: Sender<Diagnostic>,
    source: Option<String>,
}

fn format_token(maybe_tk: Option<&TokenKind>) -> String {
    match maybe_tk {
        Some(TokenKind::Identifier(_)) => String::from("<IDENTIFIER>"),
        Some(TokenKind::Bool(_)) => String::from("<BOOLEAN>"),
        Some(TokenKind::Int(_)) => String::from("<INTEGER>"),
        Some(TokenKind::Float(_)) => String::from("<FLOAT>"),
        Some(k) => k.as_ref().to_string(),
        None => String::from("EOF"),
    }
}

impl DiagnosticLogger {
    pub fn log_unexpected_eof_in_string(&mut self, range: Range) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(1)),
            code_description: None,
            source: self.source.clone(),
            message: "unexpected end of file in string, expected close quote".to_owned(),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_unrecognized_escape_code(&mut self, range: Range, c: u8) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(2)),
            code_description: None,
            source: self.source.clone(),
            message: format!("invalid control char code `{}` in string", c as char),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_invalid_unicode_code_point(&mut self, range: Range) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(3)),
            code_description: None,
            source: self.source.clone(),
            message: "invalid unicode code point following unicode control code".to_owned(),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_digit_exceeds_radix(&mut self, range: Range, radix: u8, digit: u8) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(4)),
            code_description: None,
            source: self.source.clone(),
            message: format!(
                "digit ({}) is greater than or equal to the radix ({})",
                digit, radix
            ),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_unrecognized_radix_code(&mut self, range: Range, code: u8) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(5)),
            code_description: None,
            source: self.source.clone(),
            message: format!("radix code 0{} is not recognized", code as char),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_unrecognized_character(&mut self, range: Range, character: u8) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(6)),
            code_description: None,
            source: self.source.clone(),
            message: format!("unrecognized character: `{}`", character as char),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_incomplete_unicode_or_byte_escape(&mut self, range: Range, expected_len: u32) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(7)),
            code_description: None,
            source: self.source.clone(),
            message: format!(
                "unicode or byte escape sequence requires {} hex characters immediately following",
                expected_len
            ),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_unexpected_token_specific(
        &mut self,
        range: Range,
        structure: &str,
        expected_kind: Vec<TokenKind>,
        unexpected_kind: &Option<TokenKind>,
    ) {
        let l = expected_kind.len();
        let message = match expected_kind.len() {
            0 => format!(
                "expected {} but found unexpected {}",
                structure,
                format_token(unexpected_kind.as_ref())
            ),
            k => {
                let expected_str = match k {
                    1 => format_token(Some(&expected_kind[0])),
                    _ => expected_kind.into_iter().enumerate().fold(
                        String::new(),
                        |a, (i, ref x)| match i {
                            0 => format!("one of {}", format_token(Some(x))),
                            _ if i == l - 1 => {
                                format!("{} or {}", a, format_token(Some(x)))
                            }
                            _ => format!("{}, {}", a, format_token(Some(x))),
                        },
                    ),
                };

                format!(
                    "(while parsing {}) expected {} but found unexpected {}",
                    structure,
                    expected_str,
                    format_token(unexpected_kind.as_ref())
                )
            }
        };

        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(8)),
            code_description: None,
            source: self.source.clone(),
            message,
            related_information: None,
            tags: None,
            data: None,
        })
    }

    fn log(&self, d: Diagnostic) {
        dbg!(d.clone());
        self.sender.send(d).unwrap();
        todo!();
    }

    pub fn log_duplicate_identifier(
        &mut self,
        range: Range,
        previous_range: Range,
        identifier: &str,
    ) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(9)),
            code_description: None,
            source: self.source.clone(),
            message: format!("duplicate identifier {}", identifier),
            related_information: Some(vec![DiagnosticRelatedInformation {
                location: Location::new(Uri::from_str("/").unwrap(), previous_range),
                message: format!("previous declaration of {}", identifier),
            }]),
            tags: None,
            data: None,
        })
    }

    pub fn log_pattern_must_be_annotated(&mut self, range: Range) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(10)),
            code_description: None,
            source: self.source.clone(),
            message: format!("pattern must be annotated with a type",),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_unexpected_infer_arg(&mut self, range: Range) {
        self.log(Diagnostic {
      range,
      severity: Some(DiagnosticSeverity::ERROR),
      code: Some(NumberOrString::Number(26)),
      code_description: None,
      source: self.source.clone(),
      message: "infer args may only be placed as the left hand side of a defun or the right side of an apply".to_owned(),
      related_information: None,
      tags: None,
      data: None,
    })
    }

    pub fn log_unexpected_type(&mut self, range: Range, expected_type: &str, got_type: &str) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(27)),
            code_description: None,
            source: self.source.clone(),
            message: format!(
                "expected type {}, but found type {}",
                expected_type, got_type
            ),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_not_callable(&mut self, range: Range, got_type: &str) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(28)),
            code_description: None,
            source: self.source.clone(),
            message: format!(
                "expected function type but found type {}, which is not callable",
                got_type
            ),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_unused_label(&mut self, range: Range) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(29)),
            code_description: None,
            source: self.source.clone(),
            message: "must return in label".to_owned(),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_inconsistent_return_type(
        &mut self,
        range: Range,
        expected_type: &str,
        got_type: &str,
        expected_range: Range,
    ) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(30)),
            code_description: None,
            source: self.source.clone(),
            message: format!(
                "returned type {}, which is inconsistent with other return {}",
                got_type, expected_type
            ),
            related_information: Some(vec![DiagnosticRelatedInformation {
                location: Location::new(Uri::from_str("/").unwrap(), expected_range),
                message: format!("returned type: {}", expected_type),
            }]),
            tags: None,
            data: None,
        })
    }

    pub fn log_wrong_number_type_args(
        &mut self,
        range: Range,
        fn_args: usize,
        provided_args: usize,
    ) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(31)),
            code_description: None,
            source: self.source.clone(),
            message: format!(
                "expected {} type arguments, but found {}",
                fn_args, provided_args
            ),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_cannot_be_concretized(&mut self, range: Range) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(32)),
            code_description: None,
            source: self.source.clone(),
            message: "cannot be concretized, is not a generic".to_owned(),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_duplicate_field_name(&mut self, range: Range, previous_range: Range, name: &str) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(33)),
            code_description: None,
            source: self.source.clone(),
            message: format!("duplicate field `{}`", name),
            related_information: Some(vec![DiagnosticRelatedInformation {
                location: Location::new(Uri::from_str("/").unwrap(), previous_range),
                message: format!("previous field `{}`", name),
            }]),
            tags: None,
            data: None,
        })
    }

    pub fn log_kind_mismatch(&mut self, range: Range, expected_kind: &str, got_kind: &str) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(34)),
            code_description: None,
            source: self.source.clone(),
            message: format!(
                "expected kind {}, but found kind {}",
                expected_kind, got_kind
            ),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_nonexistent_field(&mut self, range: Range, field: &str) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(35)),
            code_description: None,
            source: self.source.clone(),
            message: format!("nonexistent field `{}`", field),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_unknown_identifier(&mut self, range: Range, identifier: &str) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(37)),
            code_description: None,
            source: self.source.clone(),
            message: format!("unknown identifier `{}`", identifier),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_duplicate_use(&mut self, range: Range, previous_range: Range, prefix: &str) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(38)),
            code_description: None,
            source: self.source.clone(),
            message: format!("duplicate use for `{}`", prefix),
            related_information: Some(vec![DiagnosticRelatedInformation {
                location: Location::new(Uri::from_str("/").unwrap(), previous_range),
                message: format!("previous use `{}`", prefix),
            }]),
            tags: None,
            data: None,
        });
    }

    pub fn log_array_length_negative(&mut self, range: Range, provided_length: BigInt) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(39)),
            code_description: None,
            source: self.source.clone(),
            message: format!(
                "array length must be non-negative, but found {}",
                provided_length
            ),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_array_length_too_large(&mut self, range: Range, provided_length: BigInt) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(40)),
            code_description: None,
            source: self.source.clone(),
            message: format!(
                "array length must be less than 2^64, but found {}",
                provided_length
            ),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_num_bits_negative(&mut self, range: Range, provided_bits: BigInt) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(41)),
            code_description: None,
            source: self.source.clone(),
            message: format!("num bits must be non-negative, but found {}", provided_bits),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_num_bits_too_large(&mut self, range: Range, max_bits: u32, provided_bits: BigInt) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(42)),
            code_description: None,
            source: self.source.clone(),
            message: format!(
                "num bits must be less than {}, but found {}",
                max_bits, provided_bits
            ),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_deref_of_non_reference(&mut self, range: Range) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(43)),
            code_description: None,
            source: self.source.clone(),
            message: "cannot dereference a non-reference".to_owned(),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_empty_caseof(&mut self, range: Range) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(44)),
            code_description: None,
            source: self.source.clone(),
            message: "caseof must have at least one case".to_owned(),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_math_on_non_numeric(&mut self, range: Range) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(45)),
            code_description: None,
            source: self.source.clone(),
            message: format!("cannot perform math operation on non-numeric type"),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_comparison_on_non_numeric(&mut self, range: Range) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(46)),
            code_description: None,
            source: self.source.clone(),
            message: format!("cannot perform comparison on non-numeric type"),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_equality_on_non_integral(&mut self, range: Range) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(47)),
            code_description: None,
            source: self.source.clone(),
            message: format!("cannot perform equality comparison on non-integral type"),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_type_mismatch(&mut self, range: Range, expected_type: &str, found_type: &str) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(48)),
            code_description: None,
            source: self.source.clone(),
            message: format!(
                "type error: expected {} but found {}",
                expected_type, found_type
            ),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_int_too_large(&mut self, range: Range, nbits: u64) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(49)),
            code_description: None,
            source: self.source.clone(),
            message: format!("integer literal too large for {}-bit signed integer", nbits),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_int_too_small(&mut self, range: Range, nbits: u64) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(50)),
            code_description: None,
            source: self.source.clone(),
            message: format!("integer literal too small for {}-bit signed integer", nbits),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_uint_negative(&mut self, range: Range) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(51)),
            code_description: None,
            source: self.source.clone(),
            message: format!("unsigned integer literal cannot be negative"),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_uint_too_large(&mut self, range: Range, nbits: &BigInt) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(52)),
            code_description: None,
            source: self.source.clone(),
            message: format!(
                "unsigned integer literal too large for {}-bit unsigned integer",
                nbits
            ),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_array_access_of_non_array(&mut self, range: Range) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(53)),
            code_description: None,
            source: self.source.clone(),
            message: format!("cannot access array element of non-array type"),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_field_access_of_non_struct(&mut self, range: Range) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(54)),
            code_description: None,
            source: self.source.clone(),
            message: format!("cannot access field of non-struct type"),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_field_access_of_nonexistent_field(&mut self, range: Range, field: &str) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(55)),
            code_description: None,
            source: self.source.clone(),
            message: format!("cannot access nonexistent field `{}`", field),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_cannot_infer_array_type(&mut self, range: Range) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(56)),
            code_description: None,
            source: self.source.clone(),
            message: format!("cannot infer array type"),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_concretization_of_non_generic(&mut self, range: Range) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(57)),
            code_description: None,
            source: self.source.clone(),
            message: format!("trying to concretize non-generic"),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_struct_pattern_field_not_in_struct(&mut self, range: Range, field: &str) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(58)),
            code_description: None,
            source: self.source.clone(),
            message: format!("field `{}` not in struct", field),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_struct_pattern_on_non_struct(&mut self, range: Range) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(59)),
            code_description: None,
            source: self.source.clone(),
            message: format!("cannot destructure on non-struct type"),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_case_target_type_mismatch(
        &mut self,
        range: Range,
        expected_type: &str,
        found_pattern: &str,
    ) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(60)),
            code_description: None,
            source: self.source.clone(),
            message: format!(
                "pattern `{}` is invalid for type `{}`",
                found_pattern, expected_type,
            ),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_cannot_infer_pattern_type(&mut self, range: Range) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(61)),
            code_description: None,
            source: self.source.clone(),
            message: format!("cannot infer pattern type"),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_application_of_non_function(&mut self, range: Range, found_type: &str) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(62)),
            code_description: None,
            source: self.source.clone(),
            message: format!("cannot apply non-function `{}`", found_type),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_wrong_number_args(&mut self, range: Range, expected: usize, found: usize) {
        let message = if expected == found {
            format!("expected {} arguments, found {}", expected, found)
        } else {
            format!("expected {} arguments, found {}", expected, found,)
        };
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(63)),
            code_description: None,
            source: self.source.clone(),
            message,
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_unexpected_generic(&mut self, range: Range, expected_kind: &str) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(64)),
            code_description: None,
            source: self.source.clone(),
            message: format!("expected kind {}, but found a generic", expected_kind),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_cannot_infer_type_var_kind(&mut self, range: Range) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(65)),
            code_description: None,
            source: self.source.clone(),
            message: format!("cannot infer type variable kind"),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_cannot_infer_val_kind(&mut self, range: Range) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(66)),
            code_description: None,
            source: self.source.clone(),
            message: format!("cannot infer value variable kind"),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_cannot_infer_valpat_kind(&mut self, range: Range) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(67)),
            code_description: None,
            source: self.source.clone(),
            message: format!("cannot infer value pattern kind"),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_missing_field(&mut self, range: Range, field: &str) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(68)),
            code_description: None,
            source: self.source.clone(),
            message: format!("field `{}` not in struct", field),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_unexpected_field(&mut self, range: Range, field: &str) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(69)),
            code_description: None,
            source: self.source.clone(),
            message: format!("unexpected field `{}`", field),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_cannot_destructure_non_struct(&mut self, range: Range, found_type: &str) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(70)),
            code_description: None,
            source: self.source.clone(),
            message: format!("cannot destructure non-struct `{}`", found_type),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_cannot_new_type(&mut self, range: Range, found_type: &str) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(71)),
            code_description: None,
            source: self.source.clone(),
            message: format!("cannot new type `{}` (must be symbolic)", found_type),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_invalid_binary_op(&mut self, range: Range, op: &str, lhs: &str, rhs: &str) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(72)),
            code_description: None,
            source: self.source.clone(),
            message: format!(
                "invalid binary operation `{}` between types {} and {}",
                op, lhs, rhs
            ),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_cannot_access_field_of_non_struct(&mut self, range: Range, found_type: &str) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(73)),
            code_description: None,
            source: self.source.clone(),
            message: format!("cannot access field of non-struct `{}`", found_type),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_cannot_get_kind_of_member(&mut self, range: Range, member: &str) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(74)),
            code_description: None,
            source: self.source.clone(),
            message: format!("cannot get kind of member `{}`", member),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_cannot_get_kind_of_type(&mut self, range: Range, found_type: &str) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(75)),
            code_description: None,
            source: self.source.clone(),
            message: format!("cannot get kind of type `{}`", found_type),
            related_information: None,
            tags: None,
            data: None,
        })
    }
}
