/**
 * 
 */
package edu.umich.soar.sproom.control;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import lcmtypes.pose_t;

import sml.Identifier;

/**
 * @author voigtjr
 *
 * Configure robot.
 */
public class ConfigureCommand extends NoDDCAdapter implements Command {
	private static final Log logger = LogFactory.getLog(ConfigureCommand.class);
	static final String NAME = "configure";

	static Command newInstance(OffsetPose opose, ConfigureInterface configure) {
		return new ConfigureCommand(opose, configure);
	}
	
	private ConfigureCommand(OffsetPose opose, ConfigureInterface configure) {
		this.opose = opose;
		this.configure = configure;
	}

	private final OffsetPose opose;
	private final ConfigureInterface configure;

	@Override
	public boolean execute(Identifier command) {
		String yawFormat = command.GetParameterValue("yaw-format");
		if (yawFormat != null) {
			if (yawFormat.equals("float")) {
				configure.setFloatYawWmes(true);
			} else if (yawFormat.equals("int")) {
				configure.setFloatYawWmes(false);
			} else {
				CommandStatus.error.addStatus(command, NAME + ": Unknown format: " + yawFormat);
				return false;
			}
			logger.info(NAME + ": yaw-format set to " + yawFormat);
		}
		
		String offsetX = command.GetParameterValue("offset-x");
		String offsetY = command.GetParameterValue("offset-y");
		if (offsetX != null || offsetY != null) {
			pose_t offset = null;
			if (offsetX == null) {
				offsetX = "0";
			}
			if (offsetY == null) {
				offsetY = "0";
			}
			try {
				offset = new pose_t();
				offset.pos[0] = Double.parseDouble(offsetX);
				offset.pos[1] = Double.parseDouble(offsetY);
			} catch (NumberFormatException e) {
				CommandStatus.error.addStatus(command, NAME + ": Error parsing coordinates: " + offsetX + ", " + offsetY);
				return false;
			}
			opose.setOffset(offset);
			logger.debug(String.format("%s: offset set to x%10.3f y%10.3f", NAME, offset.pos[0], offset.pos[1]));
		}
		
		CommandStatus.accepted.addStatus(command);
		CommandStatus.complete.addStatus(command);
		return true;
	}
}
