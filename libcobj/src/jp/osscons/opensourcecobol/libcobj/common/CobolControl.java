import java.util.Optional;
import jp.osscons.opensourcecobol.libcobj.exceptions.*;

abstract class CobolControl {
	abstract public Optional<CobolControl> run() throws CobolRuntimeException, CobolGoBackException, CobolStopRunException;
	public int contId = -1;
	public CobolControl() {
		this.contId = -1;
	}
	public CobolControl(int contId) {
		this.contId = contId;
	}

	static CobolControl pure() {
		return new CobolControl() {
			public Optional<CobolControl> run() throws CobolRuntimeException, CobolGoBackException, CobolStopRunException {
				return Optional.empty();
			}
		};
	}
	
	static CobolControl goTo(CobolControl cont) {
		return new CobolControl() {
			public Optional<CobolControl> run() throws CobolRuntimeException, CobolGoBackException, CobolStopRunException {
				return cont.run();
			}
		};
	}
	
	static CobolControl performThrough(CobolControl[] contList, int begin, int end) {
		return new CobolControl() {
			public Optional<CobolControl> run() throws CobolRuntimeException, CobolGoBackException, CobolStopRunException {
				Optional<CobolControl> nextCont = Optional.of(contList[begin]);
				int executedProgramId;
				do {
					CobolControl cont = nextCont.get();
					executedProgramId = cont.contId;
					nextCont = cont.run();
				} while(nextCont.isPresent() && executedProgramId != end);
				return Optional.of(CobolControl.pure());
			}
		};
	}

	static CobolControl perform(CobolControl[] contList, int labelId) {
		return CobolControl.performThrough(contList, labelId, labelId);
	}
}