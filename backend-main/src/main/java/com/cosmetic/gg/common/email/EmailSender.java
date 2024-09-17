package com.cosmetic.gg.common.email;

import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.Properties;

import javax.mail.internet.MimeMessage;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.core.io.Resource;
import org.springframework.core.io.ResourceLoader;
import org.springframework.mail.SimpleMailMessage;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.JavaMailSenderImpl;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.stereotype.Component;
import org.springframework.util.FileCopyUtils;

import com.cosmetic.gg.authentication.jwt.JwtUtil;
import com.cosmetic.gg.common.email.dto.ActiveAccountMailSender;
import com.cosmetic.gg.common.utils.string.StringUtils;

@Component
public class EmailSender {
	
	private static final Logger log = LoggerFactory.getLogger(EmailSender.class);
	
	@Autowired
	private Environment env;
	
	@Autowired
	ResourceLoader resourceLoader;
	
	@Autowired
	private JwtUtil jwtUtil;

	@Autowired
    private JavaMailSender mailSender;
	
	public void sendMail(String toEmail, String subject, String body) {
		SimpleMailMessage message = new SimpleMailMessage();
		message.setFrom("quynhduong0911@gmail.com");
		message.setTo(toEmail);
		message.setText(body);
		message.setSubject(subject);
		
		mailSender.send(message);
		System.out.println("mail send successfully....");
	}
	
	public boolean sendEmailActiveAccount(ActiveAccountMailSender accountMailSender) {
		try {
			JavaMailSenderImpl mailSender = new JavaMailSenderImpl();
			mailSender.setHost(env.getProperty("spring.mail.host"));
			mailSender.setPort(587);
			mailSender.setUsername(env.getProperty("spring.mail.username"));
			mailSender.setPassword(env.getProperty("spring.mail.password"));
			
			Properties properties = new Properties();
			properties.setProperty("mail.smtp.auth", "true");
			properties.setProperty("mail.smtp.starttls.enable", "true");
			properties.setProperty("mail.smtp.starttls.required", "true");
//			properties.setProperty("mail.smtp.ssl.trust", "*");
			
			mailSender.setJavaMailProperties(properties);
			String from = env.getProperty("spring.mail.username");
			String to = accountMailSender.getEmail();
			
			MimeMessage message = mailSender.createMimeMessage();
			MimeMessageHelper helper = new MimeMessageHelper(message, "UTF-8");
			
			helper.setSubject("GG cosmetic - Kích hoạt tài khoản tài khoản");
			helper.setFrom(from);
			helper.setTo(to);
			
			Resource resource = resourceLoader.getResource("classpath:static/templates/activate-account-personal.txt");
			InputStream inputStream = resource.getInputStream();
			byte[] bdata = FileCopyUtils.copyToByteArray(inputStream);
			String data = new String(bdata, StandardCharsets.UTF_8);
			
			String token = jwtUtil.generateTokenActivate(accountMailSender.getUsername());
			String urlLocal = env.getProperty("send-email.url");

			String mailContent = data.replace("{{mail}}", StringUtils.nullToEmpty(to))
					.replace("{{fullname}}", StringUtils.nullToEmpty(accountMailSender.getUsername()))
					.replace("{{urlLocal}}", StringUtils.nullToEmpty(urlLocal))
					.replace("{{token}}", StringUtils.nullToEmpty(token));
			
			helper.setText(mailContent, true);
			mailSender.send(message);
			log.info("Send mail activate account success");
			return true;
		}catch(Exception ex) {
			log.error("Error while sending email to active account", ex.getCause());
			return false;
		}
	}
	
	public void sendSimpleEmail(String to) {
        SimpleMailMessage message = new SimpleMailMessage();
        message.setFrom(env.getProperty("spring.mail.username"));
        message.setTo(to);
        message.setSubject("GG cosmetic - Kích hoạt tài khoản tài khoản");
        message.setText("");
        mailSender.send(message);
        log.info("Send mail activate account success");
    }
}
