# lottery
## data 
* ```id``` 大樂透期數，格式為 ```XXXAAA```，其中 xxx 為年分、aaa 為 id
* ```n1,n2,n3,n4,n5,n6``` 是大樂透前面六個號碼
* ```sp``` 是大樂透特別號


## 猜測：剔除前 10 次開獎號碼中超過 4 次、0 次的號碼
* 會獲得命題，誰的機率大:
    * 49 個號碼，我任意選 7 個號碼，可以猜中對方選的 7 個號碼中至少中 3 個 
    * 42 個號碼，任意選 7 個號碼，可以猜中對方選的 5 個號碼中至少中 3 個 
*  結論，第一種機率比較大，為了刪除 7 個號碼而使 2 個中獎號碼刪除並不合適


