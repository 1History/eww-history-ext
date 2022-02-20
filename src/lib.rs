mod db;
mod util;
use db::{Database, Histories};
use emacs::{defun, Env, Result, Value};

const VERSION: &str = env!("CARGO_PKG_VERSION");

emacs::plugin_is_GPL_compatible!();

#[emacs::module(name = "onehistory-dyn", separator = "--")]
fn init(env: &Env) -> Result<Value<'_>> {
    env.message(format!(
        "onehistory v{VERSION} dynamic module load success!"
    ))
}

#[defun(user_ptr)]
fn open_db(db_path: String) -> Result<Database> {
    Database::open(db_path)
}

#[defun]
fn save_history(db: &Database, url: String, title: String) -> Result<()> {
    db.save_history(url, title)
}

#[defun]
fn query_histories<'a>(
    db: &Database,
    start: i64,
    end: i64,
    keyword: Option<String>,
) -> Result<Histories> {
    db.query_histories(start, end, keyword)
}

#[defun]
fn query_latest_histories<'a>(
    db: &Database,
    limit: i64,
    keyword: Option<String>,
) -> Result<Histories> {
    db.query_latest_histories(limit, keyword)
}

#[defun]
fn version(_: &Env) -> Result<String> {
    Ok(VERSION.to_string())
}
