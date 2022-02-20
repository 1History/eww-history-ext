mod db;
mod util;
use anyhow::bail;
use db::{Database, Histories, VisitDetail};
use emacs::{defun, Env, IntoLisp, Result, Value};

const MODULE_NAME: &str = "onehistory";

// Emacs won't load the module without this.
emacs::plugin_is_GPL_compatible!();

// Register the initialization hook that Emacs will call when it loads the module.
#[emacs::module(name = "onehistory")]
fn init(env: &Env) -> Result<Value<'_>> {
    env.message(format!("{MODULE_NAME} loading success!"))
}

#[defun(user_ptr)]
fn open_db(db_path: String) -> Result<Database> {
    Database::open(db_path)
}

#[defun]
fn save_history(db: &Database, url: String, title: String) -> Result<()> {
    db.persist_one_visit(url, title)
}

#[defun]
fn query_histories<'a>(
    db: &Database,
    start: i64,
    end: i64,
    keyword: Option<String>,
) -> Result<Histories> {
    let visits = db.select_visits(start, end, keyword)?;
    Ok(visits)
}
