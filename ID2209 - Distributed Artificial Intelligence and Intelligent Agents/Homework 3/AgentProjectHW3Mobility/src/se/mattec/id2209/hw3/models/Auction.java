package se.mattec.id2209.hw3.models;

import java.io.Serializable;

@SuppressWarnings("serial")
public class Auction implements Serializable {

	private long id;
	private Artifact artifact;
	private AuctionStatusEnum status;
	private int realPrice;
	private int currentBid;
	private String note;
	private String buyerName;
	private long startTime;

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

	public Artifact getArtifact() {
		return artifact;
	}

	public void setArtifact(Artifact artifact) {
		this.artifact = artifact;
	}

	public AuctionStatusEnum getStatus() {
		return status;
	}

	public void setStatus(AuctionStatusEnum status) {
		this.status = status;
	}

	public int getCurrentBid() {
		return currentBid;
	}

	public void setCurrentBid(int currentBid) {
		this.currentBid = currentBid;
	}

	public String getNote() {
		return note;
	}

	public void setNote(String note) {
		this.note = note;
	}

	public int getRealPrice() {
		return realPrice;
	}

	public void setRealPrice(int realPrice) {
		this.realPrice = realPrice;
	}

	public String getBuyerName() {
		return buyerName;
	}

	public void setBuyerName(String buyerName) {
		this.buyerName = buyerName;
	}

	public long getStartTime() {
		return startTime;
	}

	public void setStartTime(long startTime) {
		this.startTime = startTime;
	}

}
