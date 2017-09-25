DELETE FROM curator_invites
  WHERE id IN (SELECT id
  FROM (SELECT id,
  ROW_NUMBER() OVER (partition BY email ORDER BY id) AS rnum
  FROM curator_invites) t
  WHERE t.rnum > 1);
