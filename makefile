graph-render:
	stack build
	stack exec -- graph-export
	dot -Tps graph.example.student.dot -o graph.example.student.ps
