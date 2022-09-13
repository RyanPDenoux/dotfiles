;;; Compiled snippets and support files for `python-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'python-mode
                     '(("_main" "def main():\n    \"\"\"${1:Docstring}\"\"\"\n    $0\n\n\nif __name__ == '__main__':\n    main()" "main-module" t nil nil "/home/ryan/.doom.d/snippets/python-mode/main-module" nil "")
                       ("<function" "def ${1:function}():\n   \"\"\"${2:docstring}\"\"\"\n   $0" "function" t nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 'nil))
                        "/home/ryan/.doom.d/snippets/python-mode/function" nil "")
                       ("<fixture-function" "@pytest.fixture(scope=\"${1:function}\")\ndef ${2:fixture}_test_factory():\n    def _$2():\n        $0\n\n    return $2" "fixture-function" t nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 'nil))
                        "/home/ryan/.doom.d/snippets/python-mode/fixture-function" nil "")
                       ("<fixture" "@pytest.fixture(scope='${1:function}')\ndef ${2:fixture}():\n    return $0" "fixture" t nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 'nil))
                        "/home/ryan/.doom.d/snippets/python-mode/fixture" nil "")))


;;; Do not edit! File generated at Wed Feb 23 20:55:02 2022
