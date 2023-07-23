### Atopic Dermatitis Incidence Rate Prediction Model Development in South Korea: Comparisons between Regression and Artificial Neural Network

데이터 선정
- 아토피피부염 의료이용 데이터 : 국민건강보험공단
- 주민등록 인구통계 자료 : 행정안전부
- 대기오염물질측정 자료 : 에어코리아
- 종관기상관측 자료 : 기상자료개방포털

데이터 전처리
- 월별 데이터로 전처리
- 인구비율 계산(외래 건수 / 거주인원 수)

사용 기법
- Regression, Artificial Neural Network

모델 개발
- 전체 아토피피부염 발병률 예측모델(전국단위,지역단위)
- 성별 및 연령대별 아토피피부염 발병률 예측모델(전국단위,지역단위)

	Regression	ANN
	In-sample	Adjusted	Out-of-sample	Train	Valid	Test
Average	NW model	0.7939	0.7382	0.6977	0.7515	0.8628	0.8363
	AD model
without area variable	0.4220	0.4144	0.2916	0.8394	0.8124	0.7941
	AD model
with area variable	0.8913	0.8876	0.8865	0.9192	0.9062	0.9032
Gender & age under 10 years old	NW model	0.9164	0.9127	0.9008	0.9739	0.9518	0.9471
	AD model
without area variable	0.5709	0.5698	0.5688	0.8908	0.8221	0.7910
	AD model
with area variable	0.8371	0.8359	0.8303	0.9366	0.9272	0.9226
Gender & age over 10 years old	NW model	0.9092	0.9070	0.8854	0.9854	0.9745	0.9645
	AD model
without area variable	0.6772	0.6768	0.6795	0.7779	0.7632	0.7663
	AD model
with area variable	0.8035	0.8031	0.8001	0.9302	0.9305	0.9158
