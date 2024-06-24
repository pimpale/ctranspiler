use lsp_types::Position;
use lsp_types::Range;
use std::cmp::max;
use std::cmp::min;

pub struct CodeReader<Source: Iterator<Item = u8>> {
    old_pos: Position,
    position: Position,
    data: Source,
}

const FIRST_LINE_POS: u32 = 1;
const FIRST_CHAR_POS: u32 = 1;

impl<Source: Iterator<Item = u8>> Iterator for CodeReader<Source> {
    type Item = (Option<u8>, Range);

    // returns a tuple of a u8 char and the range of that char
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(byte) = self.data.next() {
            self.old_pos = self.position;

            // change line position
            if byte == b'\n' {
                self.position.line += 1;
                self.position.character = FIRST_CHAR_POS;
            } else {
                self.position.character += 1;
            }

            Some((Some(byte), Range::new(self.old_pos, self.position)))
        } else {
            Some((None, Range::new(self.old_pos, self.position)))
        }
    }
}

impl<Source: Iterator<Item = u8>> CodeReader<Source> {
    pub fn new(r: Source) -> Self {
        CodeReader {
            old_pos: Position::new(FIRST_LINE_POS, FIRST_CHAR_POS),
            position: Position::new(FIRST_LINE_POS, FIRST_CHAR_POS),
            data: r,
        }
    }
}

pub fn union_of(a: Range, b: Range) -> Range {
    Range {
        start: min(a.start, b.start),
        end: max(a.end, b.end),
    }
}
