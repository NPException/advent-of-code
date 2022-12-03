package de.npe.imageutils;

import java.io.File;

import javax.imageio.ImageIO;
import javax.imageio.stream.FileImageOutputStream;
import javax.imageio.stream.ImageOutputStream;

public final class CreateGifExample {

	public static void main(String[] args) throws Exception {
		File outFile = new File("H:/downloads/giftest/new.gif");
		File[] images = new File[] {
				  new File("H:/downloads/giftest/1.png"),
				  new File("H:/downloads/giftest/2.png"),
				  new File("H:/downloads/giftest/3.png"),
				  new File("H:/downloads/giftest/4.png"),
				  new File("H:/downloads/giftest/5.png"),
		};
		try (ImageOutputStream output = new FileImageOutputStream(outFile);
			  GifWriter writer = new GifWriter(output, 500, 0)) {
			for (File image : images) {
				writer.writeToSequence(ImageIO.read(image));
			}
		}
	}
}
