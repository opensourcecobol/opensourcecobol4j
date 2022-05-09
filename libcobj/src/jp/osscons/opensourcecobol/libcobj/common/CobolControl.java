package jp.osscons.opensourcecobol.libcobj.common;

import java.util.Optional;
import jp.osscons.opensourcecobol.libcobj.exceptions.*;

public abstract class CobolControl {
	public enum LabelType {
		label,
		section,
	};

	abstract public Optional<CobolControl> run() throws CobolRuntimeException, CobolGoBackException, CobolStopRunException;
	public int contId = -1;
	public LabelType type = LabelType.label;
	public CobolControl() {
		this.contId = -1;
		this.type = LabelType.label;
	}
	public CobolControl(int contId, LabelType type) {
		this.contId = contId;
		this.type = type;
	}

	public static CobolControl pure() {
		return new CobolControl() {
			public Optional<CobolControl> run() throws CobolRuntimeException, CobolGoBackException, CobolStopRunException {
				return Optional.empty();
			}
		};
	}
	
	public static CobolControl goTo(CobolControl cont) {
		return new CobolControl() {
			public Optional<CobolControl> run() throws CobolRuntimeException, CobolGoBackException, CobolStopRunException {
				return cont.run();
			}
		};
	}
	
	public static CobolControl performThrough(CobolControl[] contList, int begin, int end) {
		return new CobolControl() {
			public Optional<CobolControl> run() throws CobolRuntimeException, CobolGoBackException, CobolStopRunException {
				Optional<CobolControl> nextCont = Optional.of(contList[begin]);
				LabelType endType = contList[end].type;
				int executedProgramId;
				do {
					CobolControl cont = nextCont.get();
					executedProgramId = cont.contId;
					nextCont = cont.run();
				} while(nextCont.isPresent() && executedProgramId != end);

				if(endType == LabelType.section) {
					while(nextCont.isPresent() && nextCont.get().type == LabelType.label) {
						CobolControl cont = nextCont.get();
						nextCont = cont.run();
					}
				}
				return Optional.of(CobolControl.pure());
			}
		};
	}

	public static CobolControl perform(CobolControl[] contList, int labelId) {
		return CobolControl.performThrough(contList, labelId, labelId);
	}
}
