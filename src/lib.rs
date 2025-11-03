mod db;
mod util;
use db::{Database, Histories};
use emacs::{Env, Result, Value, defun};

const VERSION: &str = env!("CARGO_PKG_VERSION");

emacs::plugin_is_GPL_compatible!();

#[emacs::module(name = "eww-history-ext-dyn", separator = "--")]
fn init(env: &Env) -> Result<Value<'_>> {
    env.message(format!(
        "eww-history-ext-dyn v{VERSION} dynamic module load success!"
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
fn delete_history(db: &Database, id: i64) -> Result<()> {
    db.delete_history(id)
}

#[defun]
fn query_histories_by_range(
    db: &Database,
    start_secs: i64,
    end_secs: i64,
    keyword: Option<String>,
) -> Result<Histories> {
    db.query_histories(1000 * start_secs, 1000 * end_secs, keyword)
}

#[defun]
fn query_latest_histories(db: &Database, limit: i64, keyword: Option<String>) -> Result<Histories> {
    db.query_latest_histories(limit, keyword)
}

#[defun]
fn version(_: &Env) -> Result<String> {
    Ok(VERSION.to_string())
}
