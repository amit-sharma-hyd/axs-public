package com.adp.axs;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.Vector;

import com.pff.PSTException;
import com.pff.PSTFile;
import com.pff.PSTFolder;
import com.pff.PSTMessage;

public class ReadPst {

	public static final int WEIGHT_ONLY_RECEPIENT = 4;
	public static final int WEIGHT_ONE_OF_RECEPIENTS = 2;
	public static final int WEIGHT_INDIRECT_RECEPIENT = 1;

	public static final String TRACK_EMAIL_ID = "Sharma, Amit (CORP)";

	public static void main(String[] args) {
		new ReadPst(args[0]);
	}

	HashMap<String, Integer> weights = new HashMap<String, Integer>();

	public ReadPst(String filename) {
		try {
			filename = "/mnt/hgfs/C_Temp/2013.pst";
			PSTFile pstFile = new PSTFile(filename);
			System.out.println(pstFile.getMessageStore().getDisplayName());
			processFolder(pstFile.getRootFolder());
		} catch (Exception err) {
			err.printStackTrace();
		}
		writeOutputToFile();
		// System.out.println(weights);
	}

	private void writeOutputToFile() {
		BufferedWriter fw = null;
		try {
			fw = new BufferedWriter(new FileWriter(
					"/mnt/hgfs/C_Temp/MailOut.csv"));
			for (String name : weights.keySet()) {
				fw.write(name + "=" + weights.get(name) + "\n");
			}
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			if (fw != null) {
				try {
					fw.close();
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		}
	}

	int depth = -1;

	public void processFolder(PSTFolder folder) throws PSTException,
			java.io.IOException {
		depth++;
		// the root folder doesn't have a display name
		if (depth > 0) {
			printDepth();
			System.out.println(folder.getDisplayName());
		}

		// go through the folders...
		if (folder.hasSubfolders()) {
			Vector<PSTFolder> childFolders = folder.getSubFolders();
			for (PSTFolder childFolder : childFolders) {
				processFolder(childFolder);
			}
		}

		// and now the emails for this folder
		if (folder.getContentCount() > 0
				&& "Inbox".equals(folder.getDisplayName())) {
			depth++;
			PSTMessage email = (PSTMessage) folder.getNextChild();
			while (email != null) {
				// printDepth();

				int additionalWeight = 1;
				String toList = email.getDisplayTo();
				if (TRACK_EMAIL_ID.equals(toList)) {
					additionalWeight = WEIGHT_ONLY_RECEPIENT;
				} else if (toList.indexOf(TRACK_EMAIL_ID) != -1) {
					additionalWeight = WEIGHT_ONE_OF_RECEPIENTS;
				} else {
					additionalWeight = WEIGHT_INDIRECT_RECEPIENT;
				}

				String from = email.getSenderName();
				Integer weight = weights.get(from);
				if (weight == null) {
					weights.put(from, additionalWeight);
				} else {
					weights.put(from, weight + additionalWeight);
				}

				/*
				 * System.out.println("Email: " + email.getSubject() +
				 * ", From: " + email.getSenderName() + ", To: " +
				 * email.getDisplayTo());
				 */email = (PSTMessage) folder.getNextChild();
			}
			depth--;
		}
		depth--;
	}

	public void printDepth() {
		for (int x = 0; x < depth - 1; x++) {
			System.out.print(" | ");
		}
		System.out.print(" |- ");
	}
}