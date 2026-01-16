use crate::{
    Error, Result,
    eval::{Context, eval},
    types::{Action, Block, Map, Program, Type},
};
use std::{
    fs::File,
    io::{BufRead, BufReader, Write},
    path::PathBuf,
};

macro_rules! error {
    ( $msg:expr ) => {{
        eprintln!("Error: {}", $msg);
    }};
}

impl Program {
    pub fn process_reader<O: Write, I>(&self, reader: I, out: &mut O) -> Result<()>
    where
        I: Iterator<Item = std::io::Result<String>>,
    {
        let mut ctx = self.new_context(out)?;
        self.process_lines(&mut ctx, reader)?;
        self.finish(&mut ctx)
    }

    pub fn process_files<O: Write>(&self, files: &[PathBuf], out: &mut O) -> Result<()> {
        let mut ctx = self.new_context(out)?;
        for path in files.iter() {
            let file = BufReader::new(File::open(path)?);
            ctx.globals.insert(
                "FILENAME".to_string(),
                Type::String(path.to_string_lossy().to_string()),
            );
            self.process_lines(&mut ctx, file.lines())?;
        }
        self.finish(&mut ctx)
    }

    pub(crate) fn new_context<O: Write>(&self, out: O) -> Result<Context<O>> {
        let mut ctx = Context::new(out);
        ctx.funs = self.funs.clone();
        let data = &mut Map::new();
        for (key, expr) in self.vars.iter() {
            let val = eval(expr, data, &mut ctx)?;
            ctx.globals.insert(key.to_string(), val);
        }
        ctx.print_matched = self.print_matched;
        Ok(ctx)
    }

    pub(crate) fn process_lines<O: Write, I>(&self, ctx: &mut Context<O>, inp: I) -> Result<()>
    where
        I: Iterator<Item = std::io::Result<String>>,
    {
        for line in inp {
            ctx.line = line?;
            match self.apply(ctx) {
                Err(msg) => error!(msg),
                Ok(exit) => {
                    if exit {
                        break;
                    }
                }
            }
            ctx.index += 1;
        }
        ctx.done = true;
        Ok(())
    }

    fn apply<O: Write>(&self, ctx: &mut Context<O>) -> Result<bool> {
        for block in self.body.iter() {
            match block.run(ctx) {
                Ok(_) => (),
                Err(Error::Action(Action::Next)) => break,
                Err(Error::Action(Action::Exit)) => return Ok(true),
                err @ Err(_) => return err,
            }
        }
        Ok(false)
    }

    pub(crate) fn finish<O: Write>(&self, ctx: &mut Context<O>) -> Result<()> {
        for block in self.end.iter() {
            match block.run(ctx) {
                Ok(false) => break,
                Ok(true) => (),
                Err(Error::Action(_)) => break,
                Err(msg) => error!(msg),
            }
        }
        Ok(())
    }
}

impl Block {
    pub(crate) fn run<O: Write>(&self, ctx: &mut Context<O>) -> Result<bool> {
        let data = &mut Map::new();
        for expr in self.0.iter() {
            if !eval(expr, data, ctx)?.is_true() {
                return Ok(false);
            }
        }
        if ctx.print_matched {
            writeln!(ctx.out, "{}", ctx.line)?;
        }
        Ok(true)
    }
}
