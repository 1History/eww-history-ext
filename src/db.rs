use anyhow::{Context, Result};
use emacs::{Env, IntoLisp, Value};
use rusqlite::{named_params, Connection};

use crate::util::get_epoch_ms;

pub struct VisitDetail {
    pub url: String,
    pub title: String,
    // unix_epoch_ms
    pub visit_time: i64,
}

impl<'e> IntoLisp<'_> for VisitDetail {
    fn into_lisp(self, env: &Env) -> Result<Value<'_>> {
        env.vector((self.url, self.title, self.visit_time))
    }
}

pub struct Histories(Vec<VisitDetail>);

impl<'e> IntoLisp<'_> for Histories {
    fn into_lisp(self, env: &Env) -> Result<Value<'_>> {
        let vector = env.make_vector(self.0.len(), ())?;
        for (i, v) in self.0.into_iter().enumerate() {
            vector.set(i, v)?;
        }
        vector.into_lisp(env)
    }
}

pub(crate) struct Database {
    conn: Connection,
}

impl Database {
    pub fn open(sqlite_datafile: String) -> Result<Database> {
        let conn = Connection::open(&sqlite_datafile).context("open db")?;
        let db = Self { conn };
        db.init().context("init")?;

        Ok(db)
    }

    fn init(&self) -> Result<()> {
        self.conn
            .execute_batch(
                r#"

CREATE TABLE IF NOT EXISTS onehistory_emacs_urls (
    id integer PRIMARY KEY AUTOINCREMENT,
    url text NOT NULL UNIQUE,
    title text
);

CREATE TABLE IF NOT EXISTS onehistory_emacs_visits (
    id integer PRIMARY KEY AUTOINCREMENT,
    url_id integer,
    visit_time integer,
    UNIQUE(url_id, visit_time)
);
"#,
            )
            .context("create table")?;
        Ok(())
    }

    fn get_or_persist_url(&self, url: String, title: String) -> Result<i64> {
        let query_id = || -> rusqlite::Result<i64> {
            let mut stat = self.conn.prepare(
                r#"
         SELECT id FROM "onehistory_emacs_urls" WHERE url = :url;
"#,
            )?;
            stat.query_row(
                named_params! {
                    ":url": url,
                },
                |row| row.get(0),
            )
        };
        match query_id() {
            Err(e) if e == rusqlite::Error::QueryReturnedNoRows => {
                let mut stat = self.conn.prepare(
                    r#"
    INSERT INTO "onehistory_emacs_urls" (url, title) VALUES(:url, :title);
"#,
                )?;
                let affected = stat
                    .execute(named_params! {
                        ":url": url,
                        ":title": title,
                    })
                    .context("insert urls")?;
                assert_eq!(affected, 1);

                let id = query_id()?;
                Ok(id)
            }
            Err(e) => Err(e.into()),
            Ok(id) => Ok(id),
        }
    }

    pub fn persist_one_visit(&self, url: String, title: String) -> Result<()> {
        let url_id = self.get_or_persist_url(url, title)?;
        let mut stat = self.conn.prepare(
            r#"
    INSERT INTO "onehistory_emacs_visits" (url_id, visit_time) VALUES(:url_id, :visit_time);
"#,
        )?;
        let affected = stat
            .execute(named_params! {
                ":url_id": url_id,
                ":visit_time": get_epoch_ms(),
            })
            .context("insert visits")?;
        assert_eq!(affected, 1);

        Ok(())
    }

    pub fn select_visits(
        &self,
        start: i64,
        end: i64,
        keyword: Option<String>,
    ) -> Result<Histories> {
        let sql = format!(
            r#"
SELECT
    url,
    title,
    visit_time
FROM
    onehistory_emacs_urls u,
    onehistory_emacs_visits v ON u.id = v.url_id
WHERE
    visit_time BETWEEN :start AND :end and {}
ORDER BY
    visit_time
"#,
            Self::keyword_to_like(keyword)
        );

        let mut stat = self.conn.prepare(&sql)?;
        let rows = stat.query_map(
            named_params! {
                ":start": start,
                ":end": end,

            },
            |row| {
                let detail = VisitDetail {
                    url: row.get(0)?,
                    title: row.get(1).unwrap_or_else(|_| "".to_string()),
                    visit_time: row.get(2)?,
                };
                Ok(detail)
            },
        )?;

        let mut res: Vec<VisitDetail> = Vec::new();
        for r in rows {
            res.push(r?);
        }

        Ok(Histories(res))
    }

    fn keyword_to_like(kw: Option<String>) -> String {
        kw.map_or_else(
            || "1".to_string(),
            |v| {
                let v = v.replace("'", "");
                format!("(url like '%{v}%' or title like '%{v}%')")
            },
        )
    }
}
