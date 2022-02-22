use std::path::Path;

use anyhow::{Context, Result};
use emacs::{Env, IntoLisp, Value};
use rusqlite::{named_params, Connection};

use crate::util::get_epoch_ms;

pub struct VisitDetail {
    pub id: i64,
    pub url: String,
    pub title: String,
    pub visit_count: i64,
    // YYYY-MM-DD HH:MM:SS
    pub visit_time: String,
}

impl<'e> IntoLisp<'_> for VisitDetail {
    // translate VisitDetail into `tabulated-list-entries` format
    fn into_lisp(self, env: &Env) -> Result<Value<'_>> {
        let detail = env.vector((
            // env.call("seconds-to-time", (self.visit_time / 1000,))?,
            self.visit_time,
            self.title,
            self.url,
            self.visit_count.to_string(),
        ))?;
        env.list((self.id.to_string(), detail))
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

pub struct Database {
    conn: Connection,
}

impl Database {
    pub fn open<P: AsRef<Path>>(db_path: P) -> Result<Database> {
        let conn = Connection::open(db_path).context("open db")?;
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
    visit_count integer DEFAULT 1,
    title text
);


CREATE TABLE IF NOT EXISTS onehistory_emacs_visits (
    id integer PRIMARY KEY AUTOINCREMENT,
    url_id integer,
    visit_time integer,
    UNIQUE(url_id, visit_time)
);

CREATE VIEW IF NOT EXISTS onehistory_emacs_visits_view AS
SELECT
    v.id,
    url,
    title,
    visit_count,
    visit_time
FROM
    onehistory_emacs_urls u,
    onehistory_emacs_visits v ON u.id = v.url_id
"#,
            )
            .context("create table")?;
        Ok(())
    }

    fn get_url_id(&self, url: String, title: String) -> Result<i64> {
        let mut stat = self.conn.prepare(
            r#"
INSERT INTO onehistory_emacs_urls (url, title)
    VALUES (:url, :title)
ON CONFLICT (url)
    DO UPDATE SET
        visit_count = visit_count + 1, title = :title
    RETURNING
        id;
"#,
        )?;
        let id = stat.query_row(
            named_params! {
                ":url": url,
                ":title": title,
            },
            |row| row.get(0),
        )?;
        Ok(id)
    }

    pub fn save_history(&self, url: String, title: String) -> Result<()> {
        let url_id = self.get_url_id(url, title)?;
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

    fn query_inner(
        &self,
        start: i64,
        end: i64,
        limit: Option<i64>,
        keyword: Option<String>,
    ) -> Result<Histories> {
        let sql = format!(
            r#"
SELECT
    id,
    url,
    title,
    visit_count,
    datetime (visit_time / 1000, 'unixepoch', 'localtime')
FROM
    onehistory_emacs_visits_view
WHERE
    visit_time BETWEEN :start AND :end
    AND {}
ORDER BY
    visit_time desc
{}"#,
            Self::keyword_to_like(keyword),
            limit.map_or_else(|| "".to_string(), |lmt| format!("limit {lmt}"))
        );
        let mut stat = self.conn.prepare(&sql)?;
        let rows = stat.query_map(
            named_params! {
                ":start": start,
                ":end": end,
            },
            |row| {
                let detail = VisitDetail {
                    id: row.get(0)?,
                    url: row.get(1)?,
                    title: row.get(2).unwrap_or_else(|_| "".to_string()),
                    visit_count: row.get(3)?,
                    visit_time: row.get(4)?,
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

    pub fn query_histories(
        &self,
        start: i64,
        end: i64,
        keyword: Option<String>,
    ) -> Result<Histories> {
        self.query_inner(start, end, None, keyword)
    }

    pub fn query_latest_histories(&self, limit: i64, keyword: Option<String>) -> Result<Histories> {
        self.query_inner(0, i64::MAX, Some(limit), keyword)
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

#[cfg(test)]
mod tests {
    use tempdir::TempDir;

    use super::*;

    #[test]
    fn test_get_url_id() {
        let tmp_dir = TempDir::new("oh").unwrap();
        let db_path = tmp_dir.path().join("onehistory.db");
        let db = Database::open(db_path).unwrap();
        let cases = vec![
            ("url1", "title1", 1),
            ("url2", "title2", 2),
            ("url3", "title3", 3),
            ("url1", "title11", 1),
            ("url1", "title111", 1),
            ("url1", "title1111", 1),
        ];

        for (url, title, id) in cases {
            assert_eq!(
                id,
                db.get_url_id(url.to_string(), title.to_string()).unwrap()
            );
        }

        let cases = vec![
            ("url1", "title1111", 4),
            ("url2", "title2", 1),
            ("url3", "title3", 1),
        ];

        let mut stat = db
            .conn
            .prepare("select url, title, visit_count from onehistory_emacs_urls")
            .unwrap();
        let mut rows = stat.query([]).unwrap();
        let mut id = 1;
        while let Some(row) = rows.next().unwrap() {
            // url
            assert_eq!(
                cases[id - 1].0.to_string(),
                row.get::<_, String>(0).unwrap()
            );
            // title
            assert_eq!(
                cases[id - 1].1.to_string(),
                row.get::<_, String>(1).unwrap()
            );
            // visit_count
            assert_eq!(cases[id - 1].2, row.get::<_, i64>(2).unwrap());
            id += 1;
        }
        assert_eq!(4, id);
    }
}
