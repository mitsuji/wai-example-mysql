# wai-example-mysql

## prepare example database
```
$ mysql -u root -p
mysql> CREATE DATABASE wai_exam;
mysql> CREATE USER wai_exam_admin@localhost IDENTIFIED BY 'abcd1234';
mysql> GRANT ALL PRIVILEGES ON wai_exam.* TO wai_exam_admin@'%';
mysql> exit
```

## prepare example data
```
mysql -h localhost -D wai_exam -u wai_exam_admin -p
mysql> source sql/create.sql
mysql> source sql/insert.sql
```

## for Mac user
```
$ brew install pcre
$ brew install mysql
```

## build
```
$ stack build
```

## install
```
$ stack install
```

## execute
```
$ ~/.local/bin/wai-example-mysql-exe 0.0.0.0 9999
```

## run as script
```
$ cd app
$ stack runhaskell Main.hs 0.0.0.0 9999
```

## run in REPL
```
$ stack repl
*Main Data JSON MySQL> :main 0.0.0.0 9999 
```

## request sample for genre
```
curl -X POST \
  -d title="ジャンル4" \
  http://localhost:9999/v1/genre | jq .

curl -X DELETE \
  http://localhost:9999/v1/genre/4 | jq .

curl -X GET \
  http://localhost:9999/v1/genre | jq .

curl -X GET \
  http://localhost:9999/v1/genre/look | jq .

```

## request sample for tag
```
curl -X POST \
  -d title="タグ4" \
  http://localhost:9999/v1/tag | jq .

curl -X DELETE \
  http://localhost:9999/v1/tag/4 | jq .

curl -X GET \
  http://localhost:9999/v1/tag | jq .

curl -X GET \
  http://localhost:9999/v1/tag/look | jq .

```

## request sample for look
```
curl -X POST \
  -d title="コーデ6タイトル" \
  -d description="コーデ6説明" \
  -d genre_id=2 \
  -d tag_id="2 3" \
  http://localhost:9999/v1/look | jq .

curl -X PUT \
  -d title="コーデ6タイトル改" \
  -d description="コーデ6説明改" \
  -d genre_id=3 \
  -d tag_id="1 2 4" \
  http://localhost:9999/v1/look/6 | jq .

curl -X DELETE \
  http://localhost:9999/v1/look/6 | jq .

curl -X GET \
  http://localhost:9999/v1/look/6 | jq .

curl -X GET \
  "http://localhost:9999/v1/look" | jq .

curl -X GET \
  "http://localhost:9999/v1/look?title=%6%" | jq .

curl -X GET \
  "http://localhost:9999/v1/look?genre_id=2" | jq .

curl -X GET \
  "http://localhost:9999/v1/look?tag_id=3" | jq .

curl -X GET \
  "http://localhost:9999/v1/look?genre_id=2&tag_id=3" | jq .

```