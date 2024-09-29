# KFAlang
축구협회 프로그래밍 랭기지 KFAlang 에 오신것을 환영합니다.

```kfa
누군가 내 임기 도중 이뤄냈던 실적 에 대해 점수를 매겨보라고 한다면 10점 만점에 720점 정도는 된다고 대답하고 싶다
누군가 내 임기 도중 이뤄냈던 명예 에 대해 점수를 매겨보라고 한다면 10점 만점에 690점 정도는 된다고 대답하고 싶다
누군가 내 임기 도중 이뤄냈던 축협 에 대해 점수를 매겨보라고 한다면 10점 만점에 760점 정도는 된다고 대답하고 싶다
누군가 내 임기 도중 이뤄냈던 협회장 에 대해 점수를 매겨보라고 한다면 10점 만점에 760점 정도는 된다고 대답하고 싶다
누군가 내 임기 도중 이뤄냈던 임기 에 대해 점수를 매겨보라고 한다면 10점 만점에 320점 정도는 된다고 대답하고 싶다
누군가 내 임기 도중 이뤄냈던 협회 에 대해 점수를 매겨보라고 한다면 10점 만점에 790점 정도는 된다고 대답하고 싶다
누군가 내 임기 도중 이뤄냈던 월드컵 에 대해 점수를 매겨보라고 한다면 10점 만점에 870점 정도는 된다고 대답하고 싶다
누군가 내 임기 도중 이뤄냈던 리더십 에 대해 점수를 매겨보라고 한다면 10점 만점에 820점 정도는 된다고 대답하고 싶다
누군가 내 임기 도중 이뤄냈던 머니볼 에 대해 점수를 매겨보라고 한다면 10점 만점에 680점 정도는 된다고 대답하고 싶다

의원님께서 혹시 실적 이라는 영화 보신 적이.. 
의원님께서 혹시 명예 이라는 영화 보신 적이.. 
의원님께서 혹시 축협 이라는 영화 보신 적이.. 
의원님께서 혹시 협회장 이라는 영화 보신 적이.. 
의원님께서 혹시 협회 이라는 영화 보신 적이.. 
의원님께서 혹시 임기 이라는 영화 보신 적이.. 
의원님께서 혹시 월드컵 이라는 영화 보신 적이.. 
의원님께서 혹시 협회 이라는 영화 보신 적이.. 
의원님께서 혹시 리더십 이라는 영화 보신 적이.. 
의원님께서 혹시 협회장 이라는 영화 보신 적이.. 
의원님께서 혹시 머니볼 이라는 영화 보신 적이..
```


KFAlang은 대한축구협회의 열정과 헌신에서 영감을 받아 한국인에 적합하게 개발된 프로그래밍 언어입니다. 
축구 경기의 전략과 협회의 체계적인 운영 방식을 코드에 녹여내어, 프로그래머들이 팀워크와 리더십을 발휘하며 효율적으로 문제를 해결할 수 있도록 설계되었습니다.

KFAlang은 축구팬뿐만 아니라 모든 프로그래머들에게 새로운 도전과 재미를 선사할 것입니다. 
축구협회의 열정과 조직력을 코드로 구현한 KFAlang과 함께, 여러분의 프로그래밍 여정을 더욱 흥미롭고 의미 있게 만들어보세요.

# 사용 방법
```sh
.\kfalang.exe <source file path>
```

# 문법

## 문법 및 구문

### 변수 선언 및 할당 1

새로운 변수를 선언하고 초기 값을 설정합니다.

**구문:**
```
누군가 내 임기 도중 이뤄냈던 <변수명>에 대해 점수를 매겨보라고 한다면 <값1>점 만점에 <값2>점 정도는 된다고 대답하고 싶다
```

**구성 요소:**
- `<변수명>`: 변수의 이름.
- `<값1>점 만점에 <값2>점`: `<값2>`를 `<값1>`으로 나눈 비율을 기반으로 정수형 변수를 초기화.

**예제:**
```kfa
누군가 내 임기 도중 이뤄냈던 성과에 대해 점수를 매겨보라고 한다면 10점 만점에 8점 정도는 된다고 대답하고 싶다
```
성과라는 변수에 `(int)(8/10) = 0` 의 값으로 초기화 됩니다.

### 변수 선언 및 할당 2

기존 변수를 활용하여 새로운 변수에 값을 할당합니다.

**구문:**
```kfa
새로운 축구대표팀 <변수명1>으로|로 <변수명2|표현식>
```

**구성 요소:**
- `<변수명1>`: 할당할 변수의 이름.
- `<변수명2|표현식>`: 이미 선언된 변수명 또는 평가할 산술 표현식.

**예제:**
```kfa
새로운 축구대표팀 감독으로 홍명보
```
감독이라는 변수에 홍명보라는 변수값을 할당합니다.

### 연산자

표현식 내에서 산술 연산을 정의합니다.

**키워드 및 매핑:**
- `뭐..` → 덧셈 (`+`)
- `그..` → 뺄셈 (`-`)
- `저..` → 나눗셈 (`/`)
- `네..` → 곱셈 (`*`)

**표현식에서의 사용:**
연산자는 피연산자 사이에 사용되어 산술 연산을 수행합니다.

**예제:**
```kfa
새로운 축구대표팀 감독으로 홍명보 뭐.. 팀 그.. 축구
```
이는 `((홍명보 + 팀) - 축구)`로 해석되고, 감독이라는 변수에 해당 값을 할당합니다.

**참고**
사칙연산 우선순위 없이 앞에서 부터 해석됩니다.

### Sleep 문

지정된 시간 동안 실행을 일시 중지합니다.

**구문:**
```kfa
제가 통화 안 하고 동의를 받지 않았다는 것에는 절대 동의하지 못하겠습니다 전 전력강화위원과는 <분>분 <초>초 통화했습니다
```

**구성 요소:**
- `<분>분 <초>초`: 일시 중지할 시간을 분과 초로 지정.

**예제:**
```kfa
제가 통화 안 하고 동의를 받지 않았다는 것에는 절대 동의하지 못하겠습니다 전 전력강화위원과는 2분 30초 통화했습니다
```
이는 실행을 150초 동안 일시 중지합니다.

### 출력 문

변수의 ASCII 값에 해당하는 문자를 출력합니다.

**구문:**
```
의원님께서 혹시 <변수명>이라는|라는 영화 보신 적이..
```

**구성 요소:**
- `<변수명>`: 변수의 값을 ASCII 문자로 변환하여 출력할 변수 지정.

**예제:**
```kfa
의원님께서 혹시 머니볼이라는 영화 보신 적이..
```
이는 `머니볼` 변수의 ASCII 값에 해당하는 문자를 출력합니다.

### Return 문

지정된 종료 코드로 프로그램을 종료합니다.

**구문:**
```
결과적으로는 제 안에 있는 <변수명>가|이 나오기 시작했습니다
```

**구성 요소:**
- `<변수명>`: 종료 코드로 사용할 변수 지정.

**예제:**
```kfa
결과적으로는 제 안에 있는 무언가가 나오기 시작했습니다
```
이는 `무언가` 변수의 값을 종료 코드로 사용하여 프로그램을 종료합니다.

### While 루프

조건이 참인 동안 반복적으로 문장 블록을 실행합니다.

**구문:**
```
골 먹고 전부 다 손 들고. 이게 <변수명>이야??|야?? 
    [문장 블록]
전부 다 넘어지면 아! 아! 내가 분명히 얘기했지!
```

**구성 요소:**
- `골 먹고 전부 다 손 들고. 이게`: While 루프 시작을 알리는 키워드.
- `<변수명>이야??`: 조건 변수; 이 변수가 0이 아닐 동안 루프를 계속 실행.
- `[문장 블록]`: 루프 내에서 실행할 문장들의 목록.
- `전부 다 넘어지면 아! 아! 내가 분명히 얘기했지!`: While 루프 종료를 알리는 키워드.

**예제:**
```kfa
골 먹고 전부 다 손 들고. 이게 팀이야??
    의원님께서 혹시 머니볼이라는 영화 보신 적이..
    새로운 축구대표팀 감독으로 클린스만
전부 다 넘어지면 아! 아! 내가 분명히 얘기했지!
```

#### Break in While 루프

While 문을 빠져나옵니다.

**구문:**

```kfa
계속 정치적으로 압박을 받으면 FIFA의 제재를 받을 수 있다, 최악의 경우엔 월드컵 본선에 못 나갈 수 있다
```
또는
```kfa
제가 사퇴하겠습니다
```

**예제:**
```kfa
골 먹고 전부 다 손 들고. 이게 팀이야??
    제가 사퇴하겠습니다
전부 다 넘어지면 아! 아! 내가 분명히 얘기했지!

골 먹고 전부 다 손 들고. 이게 팀이야??
    계속 정치적으로 압박을 받으면 FIFA의 제재를 받을 수 있다, 최악의 경우엔 월드컵 본선에 못 나갈 수 있다
전부 다 넘어지면 아! 아! 내가 분명히 얘기했지!
```

**참고**
- IF 식은 별도로 지원되지 않으며, While 루프 + 변수값 변경 혹은 While 루프 + Break 구문을 활용하여 조건식을 대채합니다.
- Break 구문은 While 루프 Scope 에서만 사용할 수 있습니다.


# 이디엄
- 확장자는 *.kfa 를 권장합니다.
- `결과적으로는 제 안에 있는 무언가가 나오기 시작했습니다` 구문으로 종료하는것을 권장합니다.


# 예제
## HELLO WOLRD
```kfa
누군가 내 임기 도중 이뤄냈던 실적 에 대해 점수를 매겨보라고 한다면 10점 만점에 720점 정도는 된다고 대답하고 싶다
누군가 내 임기 도중 이뤄냈던 명예 에 대해 점수를 매겨보라고 한다면 10점 만점에 690점 정도는 된다고 대답하고 싶다
누군가 내 임기 도중 이뤄냈던 축협 에 대해 점수를 매겨보라고 한다면 10점 만점에 760점 정도는 된다고 대답하고 싶다
누군가 내 임기 도중 이뤄냈던 협회장 에 대해 점수를 매겨보라고 한다면 10점 만점에 760점 정도는 된다고 대답하고 싶다
누군가 내 임기 도중 이뤄냈던 임기 에 대해 점수를 매겨보라고 한다면 10점 만점에 320점 정도는 된다고 대답하고 싶다
누군가 내 임기 도중 이뤄냈던 협회 에 대해 점수를 매겨보라고 한다면 10점 만점에 790점 정도는 된다고 대답하고 싶다
누군가 내 임기 도중 이뤄냈던 월드컵 에 대해 점수를 매겨보라고 한다면 10점 만점에 870점 정도는 된다고 대답하고 싶다
누군가 내 임기 도중 이뤄냈던 리더십 에 대해 점수를 매겨보라고 한다면 10점 만점에 820점 정도는 된다고 대답하고 싶다
누군가 내 임기 도중 이뤄냈던 머니볼 에 대해 점수를 매겨보라고 한다면 10점 만점에 680점 정도는 된다고 대답하고 싶다

의원님께서 혹시 실적이라는 영화 보신 적이.. 
의원님께서 혹시 명예라는 영화 보신 적이.. 
의원님께서 혹시 축협이라는 영화 보신 적이.. 
의원님께서 혹시 협회장이라는 영화 보신 적이.. 
의원님께서 혹시 협회라는 영화 보신 적이.. 
의원님께서 혹시 임기라는 영화 보신 적이.. 
의원님께서 혹시 월드컵이라는 영화 보신 적이.. 
의원님께서 혹시 협회라는 영화 보신 적이.. 
의원님께서 혹시 리더십이라는 영화 보신 적이.. 
의원님께서 혹시 협회장이라는 영화 보신 적이.. 
의원님께서 혹시 머니볼이라는 영화 보신 적이..
```

```
HELLO WORLD
```

### Triangle
```kfa
누군가 내 임기 도중 이뤄냈던 골에 대해 점수를 매겨보라고 한다면 1 점 만점에 42 점 정도는 된다고 대답하고 싶다
누군가 내 임기 도중 이뤄냈던 성과에 대해 점수를 매겨보라고 한다면 1 점 만점에 10 점 정도는 된다고 대답하고 싶다

누군가 내 임기 도중 이뤄냈던 팀에 대해 점수를 매겨보라고 한다면 1 점 만점에 5 점 정도는 된다고 대답하고 싶다
누군가 내 임기 도중 이뤄냈던 일에 대해 점수를 매겨보라고 한다면 1 점 만점에 1 점 정도는 된다고 대답하고 싶다


골 먹고 전부 다 손 들고. 이게 팀이야??
    새로운 축구대표팀 협회로 팀
    골 먹고 전부 다 손 들고. 이게 협회야??
        의원님께서 혹시 골이라는 영화 보신 적이..
        새로운 축구대표팀 협회로 협회 그.. 일
    전부 다 넘어지면 아! 아! 내가 분명히 얘기했지!
    의원님께서 혹시 성과라는 영화 보신 적이..
    새로운 축구대표팀 팀으로 팀 그.. 일
전부 다 넘어지면 아! 아! 내가 분명히 얘기했지!
```

```
*****
****
***
**
*
```

### 계산 밎 Return
```kfa
누군가 내 임기 도중 이뤄냈던 골에 대해 점수를 매겨보라고 한다면 1 점 만점에 42 점 정도는 된다고 대답하고 싶다
누군가 내 임기 도중 이뤄냈던 성과에 대해 점수를 매겨보라고 한다면 1 점 만점에 10 점 정도는 된다고 대답하고 싶다

누군가 내 임기 도중 이뤄냈던 팀에 대해 점수를 매겨보라고 한다면 1 점 만점에 5 점 정도는 된다고 대답하고 싶다
누군가 내 임기 도중 이뤄냈던 일에 대해 점수를 매겨보라고 한다면 1 점 만점에 1 점 정도는 된다고 대답하고 싶다


골 먹고 전부 다 손 들고. 이게 팀이야??
    새로운 축구대표팀 협회으로 팀 뭐.. 일 그.. 골 저.. 팀 네.. 성과
    결과적으로는 제 안에 있는 협회가 나오기 시작했습니다
전부 다 넘어지면 아! 아! 내가 분명히 얘기했지!
```

```
출력없음, 
-70 반환: 정수형 계산 ((((5 + 1) - 42) / 5) * 10)
```


# 참고자료
- [축구협회 청문회 '핵심 9분' 다시 보기](https://www.youtube.com/watch?v=dz8J8XiVE6Q)
- [FParsec](https://www.quanttec.com/fparsec)
