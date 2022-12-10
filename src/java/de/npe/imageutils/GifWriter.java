/*
 * Code found here: https://memorynotfound.com/generate-gif-image-java-delay-infinite-loop-example/
 * Adapted for easier use with clojure
 */
package de.npe.imageutils;

import java.awt.image.BufferedImage;
import java.io.IOException;

import javax.imageio.IIOImage;
import javax.imageio.ImageIO;
import javax.imageio.ImageTypeSpecifier;
import javax.imageio.ImageWriteParam;
import javax.imageio.ImageWriter;
import javax.imageio.metadata.IIOMetadata;
import javax.imageio.metadata.IIOMetadataNode;
import javax.imageio.stream.ImageOutputStream;

public final class GifWriter implements AutoCloseable {

	private final ImageWriter writer;
	private final ImageWriteParam params;
	private final int delay;
	private final int loopLimit;

	private IIOMetadata metadata;

	public GifWriter(ImageOutputStream out, int delayms, int loopLimit) {
		if (loopLimit > 0x010000) {
			throw new IllegalArgumentException("loopLimit must not exceed " + 0x010000);
		}
		delay = delayms / 10;
		this.loopLimit = loopLimit;
		writer = ImageIO.getImageWritersBySuffix("gif").next();
		writer.setOutput(out);
		params = writer.getDefaultWriteParam();
	}

	public static byte[] intTo2ByteLittleEndian(int n) {
		byte[] b = new byte[2];
		b[0] = (byte) (n & 0xFF);
		b[1] = (byte) (n >> 8 & 0xFF);
		return b;
	}

	private IIOMetadata initMetaData(BufferedImage firstImage) throws IOException {
		ImageTypeSpecifier imageTypeSpecifier = ImageTypeSpecifier.createFromBufferedImageType(firstImage.getType());
		IIOMetadata metadata = writer.getDefaultImageMetadata(imageTypeSpecifier, params);

		String metaFormatName = metadata.getNativeMetadataFormatName();
		IIOMetadataNode root = (IIOMetadataNode) metadata.getAsTree(metaFormatName);

		IIOMetadataNode gceNode = getNode(root, "GraphicControlExtension");
		gceNode.setAttribute("disposalMethod", "none");
		gceNode.setAttribute("userInputFlag", "FALSE");
		gceNode.setAttribute("transparentColorFlag", "FALSE");
		gceNode.setAttribute("delayTime", Integer.toString(delay));
		gceNode.setAttribute("transparentColorIndex", "0");

		IIOMetadataNode commentsNode = getNode(root, "CommentExtensions");
		commentsNode.setAttribute("CommentExtension", "Created by: https://github.com/NPException");


		if (loopLimit != 1) {
			IIOMetadataNode appExtensionsNode = getNode(root, "ApplicationExtensions");
			IIOMetadataNode child = new IIOMetadataNode("ApplicationExtension");
			child.setAttribute("applicationID", "NETSCAPE");
			child.setAttribute("authenticationCode", "2.0");

			if (loopLimit == 0) {
				// loop continuously
				child.setUserObject(new byte[] {0x1, 0, 0});
			} else {
				// decrement loopLimit, as the GIF standard defines loops as number of iterations after the first run
				var loopBytes = intTo2ByteLittleEndian(loopLimit - 1);
				child.setUserObject(new byte[] {0x1, loopBytes[0], loopBytes[1]});
			}
			appExtensionsNode.appendChild(child);
		}
		metadata.setFromTree(metaFormatName, root);

		writer.prepareWriteSequence(null);
		return metadata;
	}

	private static IIOMetadataNode getNode(IIOMetadataNode rootNode, String nodeName) {
		int nNodes = rootNode.getLength();
		for (int i = 0; i < nNodes; i++) {
			if (rootNode.item(i).getNodeName().equalsIgnoreCase(nodeName)) {
				return (IIOMetadataNode) rootNode.item(i);
			}
		}
		IIOMetadataNode node = new IIOMetadataNode(nodeName);
		rootNode.appendChild(node);
		return node;
	}

	public void writeToSequence(BufferedImage img) throws IOException {
		if (metadata == null) {
			writer.prepareWriteSequence(metadata = initMetaData(img));
		}
		writer.writeToSequence(new IIOImage(img, null, null), params);
//		IIOMetadata metadata = this.metadata != null
//				? this.metadata
//				: (this.metadata = initMetaData(img));
//		writer.writeToSequence(new IIOImage(img, null, metadata), params);
	}

	@Override
	public void close() throws IOException {
		if (metadata != null) {
			writer.endWriteSequence();
		}
		writer.reset();
	}


	private static void time(Runnable action) {
		var start = System.nanoTime();
		action.run();
		System.out.println("Elapsed time: " + (System.nanoTime() - start) / 1000000.0 + " msecs");
	}
}
