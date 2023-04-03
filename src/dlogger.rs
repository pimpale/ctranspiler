use super::ast;
//use super::thir;
use super::token::TokenKind;
use lsp_types::Diagnostic;
use lsp_types::DiagnosticRelatedInformation;
use lsp_types::DiagnosticSeverity;
use lsp_types::Location;
use lsp_types::NumberOrString;
use lsp_types::Range;
use lsp_types::Url;
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

fn format_token(maybe_tk: Option<TokenKind>) -> String {
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
        unexpected_kind: Option<TokenKind>,
    ) {
        let message = match expected_kind.len() {
            0 => format!(
                "expected {} but found unexpected {}",
                structure,
                format_token(unexpected_kind)
            ),
            k => {
                let expected_str = match k {
                    1 => format_token(Some(expected_kind[0])),
                    _ => expected_kind
                        .into_iter()
                        .enumerate()
                        .fold(String::new(), |a, (i, x)| match i {
                            0 => format!("one of {}", format_token(Some(x))),
                            _ if i == expected_kind.len() - 1 => {
                                format!("{} or {}", a, format_token(Some(x)))
                            }
                            _ => format!("{}, {}", a, format_token(Some(x))),
                        }),
                };

                format!(
                    "(while parsing {}) expected {} but found unexpected {}",
                    structure,
                    expected_str,
                    format_token(unexpected_kind)
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

    pub fn log_duplicate_identifier(&mut self, range: Range, identifier: &str) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(9)),
            code_description: None,
            source: self.source.clone(),
            message: format!("duplicate identifier {}", identifier),
            related_information: None,
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

    // pub fn log_not_callable<A: Allocator + Clone>(
    //   &mut self,
    //   range: Range,
    //   got_type: &thir::Ty<'_, A>,
    // ) {
    //   self.log(Diagnostic {
    //     range,
    //     severity: Some(DiagnosticSeverity::ERROR),
    //     code: Some(NumberOrString::Number(28)),
    //     code_description: None,
    //     source: self.source.clone(),
    //     message: format!(
    //       "expected function type but found type {}, which is not callable",
    //       got_type
    //     ),
    //     related_information: None,
    //     tags: None,
    //     data: None,
    //   })
    // }

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
                location: Location::new(Url::parse("/").unwrap(), expected_range),
                message: format!("returned type: {}", expected_type),
            }]),
            tags: None,
            data: None,
        })
    }

    pub fn log_wrong_number_type_args(
        &mut self,
        concretization_range: Range,
        fn_args: usize,
        provided_args: usize,
    ) {
        self.log(Diagnostic {
            range: concretization_range,
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

    pub fn log_not_a_type_level_fn(&mut self, range: Range) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(32)),
            code_description: None,
            source: self.source.clone(),
            message: "cannot be concretized, is not a type level function".to_owned(),
            related_information: None,
            tags: None,
            data: None,
        })
    }

    pub fn log_duplicate_field_name(&mut self, range: Range, name: String, previous_range: Range) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(33)),
            code_description: None,
            source: self.source.clone(),
            message: format!("duplicate field `{}`", name),
            related_information: Some(vec![DiagnosticRelatedInformation {
                location: Location::new(Url::parse("/").unwrap(), previous_range),
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

    pub fn log_variable_not_found(&mut self, range: Range, identifier: &str) {
        self.log(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::Number(37)),
            code_description: None,
            source: self.source.clone(),
            message: format!("no variable `{}` found in this scope", identifier),
            related_information: None,
            tags: None,
            data: None,
        })
    }
}
