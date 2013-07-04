# 感想

- 内包表記の | の左側に値を列挙してするにはどうするの？ 以下の一行目：


    SELECT e.fname, e.lname, d.name
    FROM LEARNINGSQL.employee e JOIN LEARNINGSQL.department d
    ON e.dept_id = d.dept_id


- ! や ?! の使い分けは分かりにくい。.=. は a と Maybe a に対して多相になって欲しい。正規表現パッケージの ~= などを参考に実装できないか？


    () <- on $ e ! Employee.deptId' .=. just (d ! Department.deptId')


ではなく


    () <- on $ e ! Employee.deptId' .=. d ! Department.deptId'


- フィールド名を多相にできないか？ Lens では実現できてる？


    () <- on $ e ! deptId' .=. d ! deptId'


- query, queryMaybe は SQL と名前が違い過ぎて、意味が分からなかった。

