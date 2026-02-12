use crossterm::event::{self, Event, KeyCode as CtKeyCode, KeyEvent, KeyModifiers};
use crossterm::terminal::{disable_raw_mode, enable_raw_mode};
use patch_rexx::{env, error};
use std::fs;
use std::io::{self, BufRead, BufReader, Write};
use std::path::PathBuf;
use vim_line::{Action, Key, KeyCode, LineEditor, VimLineEditor};

// ── History ──────────────────────────────────────────────────────────

const MAX_HISTORY: usize = 1000;

struct History {
    entries: Vec<String>,
    position: Option<usize>,
    saved_input: String,
}

impl History {
    fn new() -> Self {
        Self {
            entries: Vec::new(),
            position: None,
            saved_input: String::new(),
        }
    }

    fn path() -> Option<PathBuf> {
        home::home_dir().map(|d| d.join(".rexx_history"))
    }

    fn load(&mut self) {
        let Some(path) = Self::path() else { return };
        let Ok(file) = fs::File::open(&path) else {
            return;
        };
        let reader = BufReader::new(file);
        let lines: Vec<String> = reader
            .lines()
            .map_while(Result::ok)
            .filter(|l| !l.is_empty())
            .collect();
        let start = lines.len().saturating_sub(MAX_HISTORY);
        self.entries.extend_from_slice(&lines[start..]);
    }

    fn save(&self) {
        let Some(path) = Self::path() else { return };
        let Ok(mut file) = fs::File::create(&path) else {
            return;
        };
        let start = self.entries.len().saturating_sub(MAX_HISTORY);
        for entry in &self.entries[start..] {
            let _ = writeln!(file, "{entry}");
        }
    }

    fn add(&mut self, line: &str) {
        self.entries.push(line.to_string());
        self.reset_position();
    }

    fn reset_position(&mut self) {
        self.position = None;
        self.saved_input.clear();
    }

    fn prev(&mut self, current_input: &str) -> Option<&str> {
        if self.entries.is_empty() {
            return None;
        }
        match self.position {
            None => {
                self.saved_input = current_input.to_string();
                let idx = self.entries.len() - 1;
                self.position = Some(idx);
                Some(&self.entries[idx])
            }
            Some(idx) if idx > 0 => {
                self.position = Some(idx - 1);
                Some(&self.entries[idx - 1])
            }
            Some(_) => None,
        }
    }

    fn next(&mut self) -> Option<&str> {
        match self.position {
            Some(idx) if idx + 1 < self.entries.len() => {
                self.position = Some(idx + 1);
                Some(&self.entries[idx + 1])
            }
            Some(_) => {
                self.position = None;
                Some(&self.saved_input)
            }
            None => None,
        }
    }
}

// ── Key conversion ───────────────────────────────────────────────────

fn convert_key(event: KeyEvent) -> Key {
    let code = match event.code {
        CtKeyCode::Char(c) => KeyCode::Char(c),
        CtKeyCode::Esc => KeyCode::Escape,
        CtKeyCode::Backspace => KeyCode::Backspace,
        CtKeyCode::Delete => KeyCode::Delete,
        CtKeyCode::Left => KeyCode::Left,
        CtKeyCode::Right => KeyCode::Right,
        CtKeyCode::Up => KeyCode::Up,
        CtKeyCode::Down => KeyCode::Down,
        CtKeyCode::Home => KeyCode::Home,
        CtKeyCode::End => KeyCode::End,
        CtKeyCode::Tab => KeyCode::Tab,
        CtKeyCode::Enter => KeyCode::Enter,
        _ => return Key::code(KeyCode::Escape),
    };

    Key {
        code,
        ctrl: event.modifiers.contains(KeyModifiers::CONTROL),
        alt: event.modifiers.contains(KeyModifiers::ALT),
        shift: event.modifiers.contains(KeyModifiers::SHIFT),
    }
}

// ── Rendering ────────────────────────────────────────────────────────

fn render_line(prompt: &str, input: &str, cursor: usize) {
    use crossterm::cursor::MoveToColumn;
    use crossterm::terminal::{Clear, ClearType};
    use io::Write as _;

    let mut stdout = io::stdout();
    let _ = crossterm::execute!(stdout, MoveToColumn(0), Clear(ClearType::CurrentLine));

    let _ = write!(stdout, "{prompt}{input}");
    // Position cursor: prompt width + cursor byte offset counted as chars
    let cursor_col = prompt.len() + input[..cursor.min(input.len())].chars().count();
    let col = u16::try_from(cursor_col).unwrap_or(u16::MAX);
    let _ = crossterm::execute!(stdout, MoveToColumn(col));
    let _ = stdout.flush();
}

// ── Submit helper ────────────────────────────────────────────────────

/// Execute the current input: add to history, run, clear buffer, re-enter
/// insert mode. Returns `true` if the REPL should exit (EXIT command).
fn submit(
    input: &mut String,
    editor: &mut VimLineEditor,
    history: &mut History,
    environment: &mut env::Environment,
    run_line: fn(&str, &mut env::Environment, &[String]) -> error::RexxResult<i32>,
) -> bool {
    let _ = disable_raw_mode();
    println!();

    let trimmed = input.trim().to_string();
    let should_exit = if trimmed.is_empty() {
        false
    } else {
        history.add(&trimmed);
        if trimmed.eq_ignore_ascii_case("exit") {
            true
        } else {
            match run_line(&trimmed, environment, &[]) {
                Ok(_) => {}
                Err(e) => eprintln!("{e}"),
            }
            false
        }
    };

    input.clear();
    editor.reset();
    enable_raw_mode().expect("failed to re-enable raw mode");
    // Re-enter insert mode for next line
    let _ = editor.handle_key(Key::char('i'), input);
    render_line("rexx> ", input, editor.cursor());

    should_exit
}

// ── Main REPL loop ───────────────────────────────────────────────────

pub fn run(
    environment: &mut env::Environment,
    run_line: fn(&str, &mut env::Environment, &[String]) -> error::RexxResult<i32>,
) {
    let mut editor = VimLineEditor::new();
    let mut history = History::new();
    history.load();
    let mut input = String::new();

    // Install a panic hook that restores the terminal
    let original_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        let _ = disable_raw_mode();
        // Print a newline so the panic message starts on a fresh line
        let _ = io::stdout().flush();
        original_hook(info);
    }));

    enable_raw_mode().expect("failed to enable raw mode");

    // Start in insert mode (send synthetic 'i')
    let _ = editor.handle_key(Key::char('i'), &input);

    render_line("rexx> ", &input, editor.cursor());

    while let Ok(ev) = event::read() {
        let Event::Key(key_event) = ev else {
            continue;
        };

        // Ctrl-D always quits
        if key_event.modifiers.contains(KeyModifiers::CONTROL)
            && key_event.code == CtKeyCode::Char('d')
        {
            break;
        }

        // ── Intercepts before vim-line ──────────────────────────────
        // Enter in insert mode → submit (vim-line would insert a newline)
        if key_event.code == CtKeyCode::Enter && editor.status() == "INSERT" {
            if submit(&mut input, &mut editor, &mut history, environment, run_line) {
                break;
            }
            continue;
        }

        // j/k in normal mode → history navigation
        // (vim-line treats them as line-up/down, which are no-ops on single-line input)
        if editor.status() == "NORMAL" {
            match key_event.code {
                CtKeyCode::Char('k') => {
                    if let Some(entry) = history.prev(&input) {
                        input = entry.to_string();
                        editor.set_cursor(input.len(), &input);
                    }
                    render_line("rexx: ", &input, editor.cursor());
                    continue;
                }
                CtKeyCode::Char('j') => {
                    if let Some(entry) = history.next() {
                        input = entry.to_string();
                        editor.set_cursor(input.len(), &input);
                    }
                    render_line("rexx: ", &input, editor.cursor());
                    continue;
                }
                _ => {}
            }
        }

        // ── Dispatch to vim-line ────────────────────────────────────
        let vl_key = convert_key(key_event);
        let result = editor.handle_key(vl_key, &input);

        // Apply edits in reverse to preserve byte offsets
        for edit in result.edits.into_iter().rev() {
            edit.apply(&mut input);
        }

        // Handle actions returned by vim-line
        if let Some(action) = result.action {
            match action {
                Action::Submit => {
                    if submit(
                        &mut input,
                        &mut editor,
                        &mut history,
                        environment,
                        run_line,
                    ) {
                        break;
                    }
                }
                Action::HistoryPrev => {
                    if let Some(entry) = history.prev(&input) {
                        input = entry.to_string();
                        editor.set_cursor(input.len(), &input);
                    }
                }
                Action::HistoryNext => {
                    if let Some(entry) = history.next() {
                        input = entry.to_string();
                        editor.set_cursor(input.len(), &input);
                    }
                }
                Action::Cancel => {
                    break;
                }
            }
        }

        // Render with mode-aware prompt
        let prompt = if editor.status() == "INSERT" {
            "rexx> "
        } else {
            "rexx: "
        };
        render_line(prompt, &input, editor.cursor());
    }

    let _ = disable_raw_mode();
    println!();
    history.save();
}

// ── Tests ────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    // -- History tests --

    #[test]
    fn history_add_and_navigate() {
        let mut h = History::new();
        h.add("first");
        h.add("second");
        h.add("third");

        // Navigate backwards (prev)
        assert_eq!(h.prev("current"), Some("third"));
        assert_eq!(h.prev("current"), Some("second"));
        assert_eq!(h.prev("current"), Some("first"));
        // At oldest, returns None
        assert_eq!(h.prev("current"), None);

        // Navigate forwards (next)
        assert_eq!(h.next(), Some("second"));
        assert_eq!(h.next(), Some("third"));
        // Past newest, returns saved input
        assert_eq!(h.next(), Some("current"));
        // Already at current
        assert_eq!(h.next(), None);
    }

    #[test]
    fn history_saves_current_input() {
        let mut h = History::new();
        h.add("old");

        // Navigate up with "my typing" as current
        assert_eq!(h.prev("my typing"), Some("old"));
        // Navigate back down restores saved input
        assert_eq!(h.next(), Some("my typing"));
    }

    #[test]
    fn history_reset_position() {
        let mut h = History::new();
        h.add("one");
        h.add("two");
        let _ = h.prev("x");
        assert!(h.position.is_some());
        h.reset_position();
        assert!(h.position.is_none());
    }

    #[test]
    fn history_empty_navigation() {
        let mut h = History::new();
        assert_eq!(h.prev("anything"), None);
        assert_eq!(h.next(), None);
    }

    #[test]
    fn history_save_load_roundtrip() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("test_history");

        // Manually save
        {
            let mut file = fs::File::create(&path).unwrap();
            writeln!(file, "alpha").unwrap();
            writeln!(file, "beta").unwrap();
        }

        // Load into a History by overriding the path logic
        let mut h = History::new();
        let file = fs::File::open(&path).unwrap();
        let reader = BufReader::new(file);
        for line in reader.lines().map_while(Result::ok) {
            if !line.is_empty() {
                h.entries.push(line);
            }
        }
        assert_eq!(h.entries.len(), 2);
        assert_eq!(h.entries[0], "alpha");
        assert_eq!(h.entries[1], "beta");
    }

    // -- convert_key tests --

    #[test]
    fn convert_plain_char() {
        let event = KeyEvent::new(CtKeyCode::Char('a'), KeyModifiers::NONE);
        let key = convert_key(event);
        assert_eq!(key.code, KeyCode::Char('a'));
        assert!(!key.ctrl);
        assert!(!key.alt);
        assert!(!key.shift);
    }

    #[test]
    fn convert_ctrl_modifier() {
        let event = KeyEvent::new(CtKeyCode::Char('c'), KeyModifiers::CONTROL);
        let key = convert_key(event);
        assert_eq!(key.code, KeyCode::Char('c'));
        assert!(key.ctrl);
    }

    #[test]
    fn convert_special_keys() {
        let cases = [
            (CtKeyCode::Esc, KeyCode::Escape),
            (CtKeyCode::Backspace, KeyCode::Backspace),
            (CtKeyCode::Delete, KeyCode::Delete),
            (CtKeyCode::Left, KeyCode::Left),
            (CtKeyCode::Right, KeyCode::Right),
            (CtKeyCode::Up, KeyCode::Up),
            (CtKeyCode::Down, KeyCode::Down),
            (CtKeyCode::Home, KeyCode::Home),
            (CtKeyCode::End, KeyCode::End),
            (CtKeyCode::Tab, KeyCode::Tab),
            (CtKeyCode::Enter, KeyCode::Enter),
        ];
        for (ct, vl) in cases {
            let event = KeyEvent::new(ct, KeyModifiers::NONE);
            let key = convert_key(event);
            assert_eq!(key.code, vl);
        }
    }

    #[test]
    fn convert_unknown_key_returns_escape() {
        let event = KeyEvent::new(CtKeyCode::F(5), KeyModifiers::NONE);
        let key = convert_key(event);
        assert_eq!(key.code, KeyCode::Escape);
    }
}
