package se.mattec.id2209.hw3.models;

import java.io.Serializable;

import se.mattec.id2209.hw3.models.Artifact.Genre;

@SuppressWarnings("serial")
public class User implements Serializable {

	private int interestCentury;
	private Artifact.Genre interestGenre;

	public User() {
		interestCentury = (int) (Math.random() * 500 + 1500);
		interestCentury = interestCentury - (interestCentury % 100);

		int randomGenreIndex = (int) (Math.random() * 2);
		interestGenre = Genre.values()[randomGenreIndex];
	}

	public int getInterestCentury() {
		return interestCentury;
	}

	public void setInterestCentury(int interestCentury) {
		this.interestCentury = interestCentury;
	}

	public Artifact.Genre getInterestGenre() {
		return interestGenre;
	}

	public void setInterestGenre(Artifact.Genre interestGenre) {
		this.interestGenre = interestGenre;
	}

}
