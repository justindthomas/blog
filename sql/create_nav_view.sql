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
