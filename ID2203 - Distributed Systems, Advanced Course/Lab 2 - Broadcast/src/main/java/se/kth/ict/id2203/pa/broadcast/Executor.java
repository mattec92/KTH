/**
 * This file is part of the ID2203 course assignments kit.
 * 
 * Copyright (C) 2009-2013 KTH Royal Institute of Technology
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */
package se.kth.ict.id2203.pa.broadcast;

import se.sics.kompics.launch.Scenario;
import se.sics.kompics.launch.Topology;

@SuppressWarnings("serial")
public final class Executor {

	public static final void main(String[] args) {
		Topology topology = new Topology() {
			{
				node(1, "127.0.0.1", 22031);
				node(2, "127.0.0.1", 22032);
				node(3, "127.0.0.1", 22033);
				link(1, 2, 2000, 0).bidirectional();
				link(1, 3, 6000, 0).bidirectional();
				link(2, 3, 3000, 0).bidirectional();
			}
		};

		Scenario scenario = new Scenario(Main.class) {
			{
//				command(1, "S100:B1:S10:B2:S10:B3:S3000");
//				command(1, "S100:R1:S10:R2:S10:R3:S3000");
				command(1, "S100:C1:S10:C2:S10:C3:S3000:X");
				command(2, "S10");
				command(3, "S10");
			}
		};

		scenario.executeOn(topology);
//		System.exit(0);
	}
}
