# Лабораторная Работа 2

- Выполнил: Кузьмин Артемий Андреевич
- Группа: P3314
- Вариант: oa-bag

## Содержание

- [Описание структуры данных](#описание-структуры-данных)
- [Тестирование](#тестирование)
- [Вывод](#вывод)

### Описание структуры данных

Мультимножество, реализованное с помощью хэш таблицы с открытой адресацией прпедставляет из себя множество элементов вида `<ключ>: <значение>`, где ключом является сам элемент множества, а значением - количество раз, которое он встречается в мультимножестве

#### Типы данных

```haskell
newtype Bag k = Bag [Bucket k]
```

Open-Address HashMap подразумевает, что структура данных представляет из себя динамический массив бакетов. В свою очередь каждый бакет может хранить только один элемент

```haskell
data Bucket k = Bucket k Int | EmptyBucket deriving (Eq)
```

Реализованная структура также является **моноидом**

```haskell
instance (Eq k, Show k) => Semigroup (Bag k) where
    (<>) (Bag b1) (Bag b2) = foldlBag f (Bag b1) (Bag b2)
      where
        f acc b = case b of
            EmptyBucket -> acc
            Bucket k v -> insertN k v acc

instance (Eq k, Show k) => Monoid (Bag k) where
    mempty = newBag
    mappend = (<>)
```

В данном случае нейтральным элементом является `newBag` - пустое мультимножество с 10 бакетами, а ассоциативной операцией слияние двух мультимножеств

> [!NOTE]
> Данная реализация мультимножества является полиморфной и может хранить различные типы данных. Хранимый тип данных должен реализовывать классы Eq (необходим для сравнения мультимножеств) и Show (необходим для хэширования элемеентов)

#### Методы

Доступные методы

- **insert** - вставка
- **delete** - удаление
- **count** - счет
- **filterBag** - фильтрация
- **foldlBag** - левая сверка
- **foldrBag** - правая свертка
- **mapBag** - отображение

#### Вставка

```haskell
insert :: (Eq k, Show k) => k -> Bag k -> Bag k
insert key (Bag buckets) =
    if loadFactor (Bag buckets) >= 0.7
        then insert key (resize $ Bag buckets)
        else updatedBag
  where
    helper i = case buckets !! i of
        EmptyBucket -> updateBucket buckets i key
        Bucket k v ->
            if k == key || v < 1
                then updateBucket buckets i key
                else helper ((i + 1) `mod` length buckets)
    h = hashcode key `mod` length buckets
    updatedBag = Bag $ helper h
```

Вставка реализуется на основе хэш функции с линейным пробированием. Если коэффициент загрузки перед вставкой больше, чем 0.7, то сначала произойдет автоматическое расширение массива и уже потом вставка

##### Коэффициент загрузки

```haskell
loadFactor :: (Eq k) => Bag k -> Double
loadFactor (Bag buckets) = filled / total
  where
    filled = fromIntegral . length $ filter (/= EmptyBucket) buckets
    total = fromIntegral $ length buckets
```

##### Расширение массива

```haskell
resize :: (Eq k, Show k) => Bag k -> Bag k
resize (Bag buckets) = reinsertAll entries (Bag [EmptyBucket | _ <- [1 .. newSize]])
  where
    newSize = length buckets * 2
    entries = [(k, v) | Bucket k v <- buckets]
    reinsertAll [] bag = bag
    reinsertAll ((k, v) : es) bag = reinsertAll es (insertN k v bag)
```

#### Удаление

```haskell
delete :: (Eq k) => (Show k) => k -> Bag k -> Bag k
delete key (Bag buckets) = Bag $ helper h
  where
    helper i = case buckets !! i of
        EmptyBucket -> decrementBucket buckets i key
        Bucket k _ ->
            if k == key
                then decrementBucket buckets i key
                else helper ((i + 1) `mod` length buckets)
    h = hashcode key `mod` length buckets
```

Просто удалить элемент нельзя, так как это может привести к ошибке при поиске элемента. Поэтому, сам ключ все ещё остается в мультимножестве, однако значение для этого ключа становится 0

#### Счет

```haskell
count :: (Eq k) => (Show k) => k -> Bag k -> Int
count key (Bag buckets) = helper h
  where
    helper i = case buckets !! i of
        EmptyBucket -> 0
        Bucket k v ->
            if k == key
                then v
                else helper ((i + 1) `mod` length buckets)
    h = hashcode key `mod` length buckets
```

Вывод значения по ключу или 0, если ключа нет

#### Фильтрация

```haskell
filterBag :: (Eq k) => (Bucket k -> Bool) -> Bag k -> Bag k
filterBag _ (Bag []) = Bag []
filterBag predicate (Bag (b : bs))
    | b == EmptyBucket || predicate b = Bag (b : filteredBs)
    | otherwise = case b of
        Bucket k _ -> Bag $ Bucket k 0 : filteredBs
        EmptyBucket -> Bag $ EmptyBucket : filteredBs
  where
    Bag filteredBs = filterBag predicate (Bag bs)
```

Фильтрация избегает уменьшения массива, так как это может привести к ошибкам при поиске элементов

#### Левая и правая свертки

```haskell
foldlBag :: (a -> Bucket k -> a) -> a -> Bag k -> a
foldlBag _ acc (Bag []) = acc
foldlBag f acc (Bag (b : bs)) = foldlBag f (f acc b) (Bag bs)

foldrBag :: (Bucket k -> a -> a) -> a -> Bag k -> a
foldrBag _ acc (Bag []) = acc
foldrBag f acc (Bag (b : bs)) = f b (foldrBag f acc (Bag bs))
```

#### Отображение

```haskell
mapBag :: (Bucket k -> Bucket k) -> Bag k -> Bag k
mapBag _ (Bag []) = Bag []
mapBag f (Bag (b : bs)) = case mapBag f (Bag bs) of
    Bag mappedBs -> Bag (f b : mappedBs)
```

### Тестирование

#### Модульное тестирование

Проводилось с помощью библиотеки Tasty

```haskell
unitTests :: TestTree
unitTests =
    testGroup
        "Unit Tests"
        [ testCase "Insert and Count" $
            let bag = insert "apple" . insert "apple" . insert "banana" $ newBag
             in count "apple" bag @?= 2,
          testCase "Delete" $
            let bag = delete "apple" . insert "apple" . insert "apple" $ newBag
             in count "apple" bag @?= 1,
          testCase "Delete non-existing key" $
            let bag = delete "orange" . insert "apple" $ newBag
             in count "apple" bag == 1 && count "orange" bag == 0 @?= True,
          testCase "FilterBag" $
            let bag = insert "apple" . insert "banana" . insert "apple" $ newBag
                f b = case b of
                    Bucket k _ -> k == "apple"
                    EmptyBucket -> True
                filteredBag = filterBag f bag
             in count "apple" filteredBag == 2 && count "banana" filteredBag == 0 @?= True,
          testCase "Automatic resize on insert" $
            let bag = foldl (flip insert) newBag [1 :: Int .. 10]
                Bag buckets = bag
             in length buckets @?= 20,
          testCase "FoldlBag" $
            let bag = foldl (flip insert) newBag [1 :: Int .. 10]
                f acc b = case b of
                    EmptyBucket -> acc
                    Bucket _ v -> acc + v
             in foldlBag f 0 bag @?= 10,
          testCase "FoldrBag" $
            let bag = foldl (flip insert) newBag [1 :: Int .. 10]
                f b acc = case b of
                    EmptyBucket -> acc
                    Bucket _ v -> acc + v
             in foldrBag f 0 bag @?= 10,
          testCase "MapBag" $
            let bag = foldl (flip insert) newBag [1 :: Int .. 10]
                mapFunc b = case b of
                    EmptyBucket -> EmptyBucket
                    Bucket k v -> Bucket k (v * 2)
                foldlFunc acc b = case b of
                    EmptyBucket -> acc
                    Bucket _ v -> acc + v
             in foldlBag foldlFunc 0 (mapBag mapFunc bag) @?= 20
        ]
```

#### Property-based тестирование

Проводилось с помощью библиотеки QuickCheck

Для генерации случайных мультимножеств необходимо объявить представителя класса Arbitrary

```haskell
instance (Arbitrary k, Show k, Eq k, Num k, Random k) => Arbitrary (Bag k) where
    arbitrary = do
        pairs <- listOf $ (,) <$> arbitrary <*> choose (0, 5)
        return $ foldr (\(k, n) bag -> insertN k n bag) newBag pairs
```

Реализованные законы

```haskell
prop_identityLaw :: Bag Int -> Bool
prop_identityLaw bag = (mempty <> bag) == (bag <> mempty)

prop_associativeLaw :: Bag Int -> Bag Int -> Bag Int -> Bool
prop_associativeLaw x y z =
    ((x <> y) <> z) == (x <> (y <> z))

prop_insertLaw :: Bag Int -> Int -> Bool
prop_insertLaw b e = numOfElems b + 1 == numOfElems (insert e b)
  where
    numOfElems = foldlBag f 0
    f acc bag = case bag of
        EmptyBucket -> acc
        Bucket _ v -> acc + v
```

### Вывод

В ходе работы я узнал как создавать свои типы данных в haskell и как сделать их представителями других классов. Также я познакомился с property-based тестированием. Оно позволяет абстрагироваться от конкретных значений и проверить функциональности API, а не поведение в конкретном случае, что иногда позволяет находить неочевидные ошибки
