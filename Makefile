#Referenced 'https://medium.com/@mounirboulwafa/creating-a-single-executable-file-exe-from-a-python-program-abda6a41f74f'

all:
	pip install pyinstaller
	pyinstaller --onefile --windowed while.py
	mv dist/while .
	rm -rf while.spec
	rm -rf ./dist
	rm -rf ./build
