for f in `find . -name '*.ml*'` ; \
do ( \
ocp-indent -i $f; \
); \
done

