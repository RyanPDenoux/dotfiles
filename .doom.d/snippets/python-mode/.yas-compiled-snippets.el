;;; Compiled snippets and support files for `python-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'python-mode
                     '(("<ut" "def test_${1:object}_${2:behavior}_${3:expectation}(${4:fixtures}):\n    $0\n" "pytest-unit-test" t nil nil "/home/ryan/.doom.d/snippets/python-mode/pytest-unit-test" nil "")
                       ("<utff" "@pytest.fixture(scope=\"${1:function$$(yas-choose-value '(\\\"function\\\" \\\"class\\\" \\\"module\\\" \\\"package\\\" \\\"session\\\"))}\")\ndef ${2:fixture-name}_test_factory(${3:fixtures}):\n    def _$2(${4:params}):\n        $0\n\n    return _$2\n" "pytest-fixture-function" t nil nil "/home/ryan/.doom.d/snippets/python-mode/pytest-fixture-function" nil "")
                       ("<utf" "@pytest.fixture(scope=\"${1:function$$(yas-choose-value '(\\\"function\\\" \\\"class\\\" \\\"module\\\" \\\"package\\\" \\\"session\\\"))}\")\ndef ${2:fixture-name}(${3:fixtures}):\n    $0\n" "pytest-fixture" t nil nil "/home/ryan/.doom.d/snippets/python-mode/pytest-fixture" nil "")
                       ("_main" "def main():\n    \"\"\"${1:Docstring}\"\"\"\n    $0\n\n\nif __name__ == '__main__':\n    main()" "main-module" t nil nil "/home/ryan/.doom.d/snippets/python-mode/main-module" nil "")
                       ("<function" "def ${1:function}():\n   \"\"\"${2:docstring}\"\"\"\n   $0" "function" t nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 'nil))
                        "/home/ryan/.doom.d/snippets/python-mode/function" nil "")
                       ("<fixture-function" "@pytest.fixture(scope=\"${1:function}\")\ndef ${2:fixture}_test_factory():\n    def _$2():\n        $0\n\n    return $2" "fixture-function" t nil
                        ((yas-indent-line 'fixed))
                        "/home/ryan/.doom.d/snippets/python-mode/fixture-function" nil "")
                       ("<fixture" "@pytest.fixture(scope='${1:function}')\ndef ${2:fixture}():\n    return $0" "fixture" t nil
                        ((yas-indent-line 'fixed))
                        "/home/ryan/.doom.d/snippets/python-mode/fixture" nil "")))


;;; Do not edit! File generated at Tue Nov  1 10:47:52 2022
