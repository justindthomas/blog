CREATE TABLE "article" (
  "id" BIGSERIAL PRIMARY KEY UNIQUE,
  "reference" VARCHAR NOT NULL,
  "title" VARCHAR NOT NULL,
  "summary" VARCHAR NOT NULL,
  "content" VARCHAR NOT NULL,
  "front_page" BOOLEAN NOT NULL DEFAULT true,
  "created_at" TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()
);

ALTER TABLE "article" ADD CONSTRAINT "reference_uniq" UNIQUE("reference");

CREATE TABLE "facebook_user" (
  "id" BIGSERIAL PRIMARY KEY UNIQUE,
  "user_id" VARCHAR NOT NULL,
  "user_name" VARCHAR NOT NULL
);

ALTER TABLE "facebook_user" ADD CONSTRAINT "id_uniq" UNIQUE("user_id");

CREATE TABLE "files" (
  "id" BIGSERIAL PRIMARY KEY UNIQUE,
  "file_name" VARCHAR NOT NULL,
  "file_data" BYTEA NOT NULL,
  "content_type" VARCHAR NOT NULL
);

ALTER TABLE "files" ADD CONSTRAINT "filename_uniq" UNIQUE("file_name");

CREATE VIEW navigation AS (
WITH sub AS (
SELECT
  DISTINCT a.id AS id,
  COALESCE(LAG(a.reference)         OVER (ORDER BY a.created_at DESC)
          ,FIRST_VALUE(a.reference) OVER (ORDER BY a.created_at DESC ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING)) AS next,
  COALESCE(LEAD(a.reference)        OVER (ORDER BY a.created_at DESC)
          ,LAST_VALUE(a.reference)  OVER (ORDER BY a.created_at DESC ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING)) AS prev,
  a.created_at AS created_at
FROM article a
WHERE a.front_page = true
ORDER BY a.created_at DESC
)
SELECT
  a.id AS id,
  a.reference AS reference,
  a.title AS title,
  a.summary AS summary,
  a.content AS content,
  a.created_at AS created_at,
  a.front_page AS front_page,
  s.next AS next,
  s.prev AS prev
FROM article a
LEFT JOIN sub s
ON a.id = s.id
);
